#!/usr/bin/env python
import os
import sys
if os.environ.get("REZ_USED"):
    import rez
else:
    sys.path.append("/data/rez/install/2.13.0/lib/python2.7/site-packages/rez-2.13.0-py2.7.egg")
    import rez
import shutil
from datetime import datetime
import inspect
from rez.build_process_ import create_build_process, BuildType
from rez.build_system import create_build_system

import xml.dom.minidom as dom

from pprint import pprint

class ClionWorkspace(object):
    """
    A Class to handle manipulation of clions workspace xml file.
    """
    def __init__(self, workspace_file, backup=True):
        self._workspace_file = workspace_file
        if backup:
            self.backup(workspace_file)
        self._tmpfile = workspace_file+".tmp"
        self._dom = dom.parse(workspace_file)

    def find(self, *args, **kwargs):
        """
        Find either a comonent from args or a attribute from kwargs
        """
        if args:
            element = self._dom.getElementsByTagName(args[0])
            if element:
                return element[0]
        if 'name' in kwargs:
            components = self._dom.getElementsByTagName("component")
            for element in components:
                if element.getAttribute('name') == kwargs["name"]:
                    return element
        return None

    def __getattr__(self, key):
        if key == "cmake":
            self.find(name="CMakeSettings")
            return "cmake"
        return None

    @property
    def CMakeSettings(self):
         return self.find(name="CMakeSettings")[0][0].getAttribute('GENERATION_OPTIONS')

    @CMakeSettings.setter
    def CMakeSettings(self, value):
        element = self.find(name="CMakeSettings")
        if element:
            element.setAttribute("GENERATION_OPTIONS", value)
        return None

    @property
    def ENVS(self, value):
        pass

    @ENVS.setter
    def ENVS(self, value):
        element = self.find("ADDITIONAL_GENERATION_ENVIRONMENT")
        current_envs = None
        for child in element.childNodes:
            if child.nodeType == dom.Node.ELEMENT_NODE:
                if child.tagName == "envs":
                    current_envs = child
        envs = dom.Element("envs")
        for k,v in value.iteritems():
            el = dom.Element("env")
            el.setAttribute("name", k)
            el.setAttribute("value", v)
            envs.appendChild(el)
        element.replaceChild(envs, current_envs)

    def backup(self, orig_file):
        timestamp = datetime.now().strftime("%y%m%d")
        self.backup_file = orig_file+"."+timestamp
        print "Backing up workspace file to:", self.backup_file
        shutil.copy2(orig_file, self.backup_file)

    def write(self):
        print "Writing workspace file to:", self._workspace_file
        self._dom.writexml(open(self._workspace_file, 'wb'))


def rez_root():
    """
    Check if current dir is a valid package.
    """
    if os.path.exists(os.path.join(os.getcwd(), "package.py")):
        return os.getcwd()
    return None

def clion_workspace():
    """
    Check if current dir is a valid clion root.
    """
    root = os.path.join(os.getcwd(), ".idea", "workspace.xml")
    if os.path.exists(root):
        return root
    return None

def build_information(working_dir):
    """
    Collect information from the rez build system.
    """
    working_dir = os.getcwd()
    buildsys_type = None
    buildsys = create_build_system(
        working_dir,
        buildsys_type=buildsys_type,
        opts=[],
        write_build_scripts=False,
        verbose=False,
        build_args="",
        child_build_args="")
    builder = create_build_process(
        "local",
        working_dir,
        build_system="cmake",
        verbose=False)
    # Just grab the first variant for now.
    variant = None
    for v in builder.package.iter_variants():
        if v:
            variant = v
            break
    variant_build_path = builder.build_path
    if not os.path.exists(variant_build_path):
        os.makedirs(variant_build_path)
    context, rxt_filepath = builder.create_build_context(
            variant=variant,
            build_type=BuildType.local,
            build_path=builder.build_path)
    return buildsys, context

def main():
    package_root = rez_root()
    # clion_workspace_file = clion_workspace()
    if not package_root:
        return 1

    buildsys, context = build_information(package_root)

    # clion = ClionWorkspace(clion_workspace_file)
    print buildsys
    print ' '.join(x.name for x in context.requested_packages(True))
    module_paths = context.get_environ()['CMAKE_MODULE_PATH']+";"+os.path.join(os.path.join(os.path.dirname(inspect.getfile(buildsys.__class__))),"cmake_files")
    print "module paths:\n " + module_paths
    # clion.CMakeSettings = " ".join(buildsys.settings.cmake_args+["-DCMAKE_MODULE_PATH="+module_paths])
    CMakeSettings = " ".join(buildsys.settings.cmake_args+["-DCMAKE_MODULE_PATH="+module_paths])
    env = context.get_environ()
    env.update({
        "REZ_BUILD_PROJECT_VERSION": str(buildsys.package.version),
        "REZ_BUILD_PROJECT_NAME": buildsys.package.name,
        "REZ_BUILD_REQUIRES_UNVERSIONED": ' '.join(x.name for x in context.requested_packages(True))
        })
    print "Envs:"
    print env
    # clion.ENVS = env

    # clion.write()

if __name__ == '__main__':
    sys.exit(int(main() or 0))
