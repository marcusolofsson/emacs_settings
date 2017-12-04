#!/bin/env python
"""
Utility to get information about a rez package.
It will print a json formated dict to stdout, that can be used in other applications.

Author: Fredrik Braennbacka (fredrik.brannbacka@ilpvfx.com)
"""
import json
import sys
import os
from rez.packages_ import get_developer_package
from rez.resolved_context import ResolvedContext
from rez.utils.formatting import PackageRequest
from rez.exceptions import PackageCommandError, PackageMetadataError
from rez.rex_bindings import VersionBinding, VariantBinding, VariantsBinding
from rez.utils.sourcecode import SourceCodeError
from rez.rex import RexExecutor, Python
from rez.config import config
from rez.build_system import create_build_system
from rez.build_process_ import BuildType
import argparse
from pprint import pprint as pp

class IDEExecutor:
    br = '#' * 80
    br_minor = '-' * 80

    def __init__(self, executor, build=False):
        self.build = build
        self._ex = executor
        self._heading("ILP EDIT CONTEXT")

    @classmethod
    def executor(cls, build=False):

        interp = Python(target_environ={}, passive=True)
        executor = RexExecutor(
            interpreter=interp, parent_environ={}, parent_variables=True)
        return cls(executor, build)

    def _heading(self, txt):
        self._ex.comment("")
        self._ex.comment("")
        self._ex.comment(self.br)
        self._ex.comment(txt)
        self._ex.comment(self.br)

    def _minor_heading(self, txt):
        self._ex.comment("")
        self._ex.comment(txt)
        self._ex.comment(self.br_minor)

    def add_context(self, context, variant):
        # bind various info to the execution context
        resolved_pkgs = context.resolved_packages or []
        request_str = ' '.join(str(x) for x in context._package_requests)
        implicit_str = ' '.join(str(x) for x in context.implicit_packages)
        resolve_str = ' '.join(x.qualified_package_name for x in resolved_pkgs)
        package_paths_str = os.pathsep.join(context.package_paths)

        self._heading("system setup")
        self._ex.setenv("REZ_USED", context.rez_path)
        self._ex.setenv("REZ_USED_VERSION", context.rez_version)
        self._ex.setenv("REZ_USED_TIMESTAMP", str(context.timestamp))
        self._ex.setenv("REZ_USED_REQUESTED_TIMESTAMP",
                        str(context.requested_timestamp or 0))
        self._ex.setenv("REZ_USED_REQUEST", request_str)
        self._ex.setenv("REZ_USED_IMPLICIT_PACKAGES", implicit_str)
        self._ex.setenv("REZ_USED_RESOLVE", resolve_str)
        self._ex.setenv("REZ_USED_PACKAGES_PATH", package_paths_str)

        if self.build:
            self._heading("build setup")

        # binds objects such as 'request', which are accessible before resolve
        bindings = context._get_pre_resolve_bindings()
        for k, v in bindings.items():
            self._ex.bind(k, v)

        self._ex.bind('resolve', VariantsBinding(resolved_pkgs))

        #
        # -- apply each resolved package to the execution context
        #

        self._heading("package variables")
        error_class = SourceCodeError if config.catch_rex_errors else None

        # set basic package variables and create per-package bindings
        bindings = {}
        for pkg in resolved_pkgs:
            bindings[pkg.name] = dict(version=VersionBinding(pkg.version),
                                      variant=VariantBinding(pkg))

        resolved_pkgs.append(variant.parent)
        bindings[variant.parent.name] = dict(
            version=VersionBinding(variant.version),
            variant=VariantBinding(variant))

        # commands
        for attr in ("pre_commands", "commands", "post_commands"):
            found = False
            for pkg in resolved_pkgs:
                commands = getattr(pkg, attr)
                if commands is None:
                    continue
                if not found:
                    found = True
                    self._heading(attr)

                self._minor_heading("%s from package %s" % (attr, pkg.name))
                bindings_ = bindings[pkg.name]
                self._ex.bind('this',       bindings_["variant"])
                self._ex.bind("version",    bindings_["version"])
                self._ex.bind('root',       pkg.root)
                self._ex.bind('base',       pkg.base)

                exc = None
                commands.set_package(pkg)

                try:
                    self._ex.execute_code(commands, isolate=True)
                except error_class as e:
                    exc = e

                if exc:
                    header = "Error in %s in package %r:\n" % (attr, pkg.uri)
                    if context.verbosity >= 2:
                        msg = header + str(exc)
                    else:
                        msg = header + exc.short_msg

                    raise PackageCommandError(msg)

        self._heading("post system setup")

        # append suite paths based on suite visibility setting
        # self._append_suite_paths(executor)

        # append system paths
        self._ex.append_system_paths()

    def list_actions(self):
        for action in self._ex.actions:
            print action

    def get_output(self):
        return self._ex.get_output()

def get_dependencies(context, ignore=None):
    """
    Get a dict of python dependencies for the provided context.

    :param context: A resolved context
    :return: A dict with library names and their corresponding path
    """
    result = {}
    for path in context.get_environ()["PYTHONPATH"].split(':'):
        matches = list(filter(lambda x: x.root in path, context.resolved_packages))
        if not matches:
            continue
        package = matches[0]
        if ignore is not None and package.name == ignore.name:
            continue
        result[package.qualified_package_name] = path
    return result

def get_environment(context):
    new_env = {}
    for key, value in context.get_environ().iteritems():
        # if key not in os.environ and key is not "CMAKE_MODULE_PATH":
        # sys.stderr.write("%s = %s\n" % (key, value))
        new_env[key] = value
    return new_env

def get_package_info(path, variant_index, build_type=None):
    """
    Get valuable information about a package that can be used in different contexts.

    :param path: Path to the root of a project
    :param variant: index of the variant to resolve
    :return: Dict with various info about a package
    """
    data = {
        "variants": [],
    }
    package = get_developer_package(path)
    variants = list(package.iter_variants())
    for v in variants:
        data["variants"].append(v.qualified_name)
    variant = variants[variant_index]
    request = variant.get_requires(build_requires=True, private_build_requires=True)
    #request.append(PackageRequest(package.name))
    context = ResolvedContext(request, building=True)
    data["name"] = package.name
    data["interpreter"] = context.which("python")
    data["dependencies"] = get_dependencies(context, ignore=package)
    data["environment"] = get_environment(context)
    if build_type == "cpp":
        from rez.plugin_managers import plugin_manager
        cmake = plugin_manager.get_plugin_module('build_system', 'cmake')
        cmake_dir = os.path.join(os.path.dirname(cmake.__file__), "cmake_files")
        data["environment"]["CMAKE_MODULE_PATH"] += ";"+cmake_dir
        data["environment"]["REZ_BUILD_PROJECT_FILE"] = package.filepath
        data["environment"]["REZ_BUILD_PROJECT_VERSION"] = str(package.version)
        data["environment"]["REZ_BUILD_PROJECT_NAME"] = package.name
        # sys.stderr.write("CMAKE_DIR: "+cmake_dir)

    #executor = IDEExecutor.executor(build=True)
    #executor.add_context(context, variant)
    #sys.stderr.write(str(executor.get_output()))
    buildsys = create_build_system(path,
                                   buildsys_type=None,
                                   opts=[],
                                   write_build_scripts=False,
                                   verbose=True,
                                   build_args="",
                                   child_build_args="")
    data['build'] = buildsys.get_standard_vars(context, variant, BuildType.local, False, path, False)
    data['environment'].update(data['build'])
    # sys.stderr.write(str(buildsys.get_standard_vars(context, variant, BuildType.local, False, path, False)))
    # pp(buildsys.get_standard_vars(context, variant, BuildType.local, False, path, False))
    return data


if __name__ == "__main__":
    # sys.stderr.write("args: %s" % (str(sys.argv)))
    parser = argparse.ArgumentParser(description='Rez info.')
    parser.add_argument('root', default=os.getcwd(), help="path to the root of package")
    parser.add_argument('build_type', type=str, choices=["python", "cpp"], default="cpp", help="variant to resolve")
    parser.add_argument('--variant', type=int, default=0, help="variant to resolve")


    args = parser.parse_args()
    data = get_package_info(args.root, args.variant, args.build_type)
    # print json.dumps(data, indent=2)
    # pp(data['environment'])
    # pp(data['build'])
    data_str = ""
    for k, v in data['environment'].iteritems():
        data_str += k + ":" + str(v) + " "
    print data_str
