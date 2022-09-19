#!/usr/bin/python3

#  Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.

import json
import sys
import os
import subprocess

def get_json_item(fsm, key):
    value = fsm.get(key)
    if value == None:
        print("Missing tag \"" + key + "\"\n")
        exit(0)
    return value

def print_indent(out_file, language):
    if language == "java":
        out_file.write("    ")
    elif language == "c/c++":
        out_file.write("  ")
    return

arg_size = len(sys.argv)
if arg_size < 2 or arg_size > 3:
    print("Usage: " + sys.argv[0] + " file [-archive]\n")
    exit(0)

# load fsm model declare json file
fsm_file = open(sys.argv[1], "r", encoding="utf-8")
fsm = json.load(fsm_file)

# process source language type
language = get_json_item(fsm, "source_language")
language = language.lower()

# read items from json file
fsm_name = get_json_item(fsm, "fsm_name")
if fsm_name == "":
    print("\"fsm_name\" is empty\n")
    exit(0)

# write target file headers
out_file_name = ""
if language == "java":
    out_file_name = "fsm_" + fsm_name + "."
    out_file_name = out_file_name.upper() + "java"
elif language == "c/c++":
    out_file_name = fsm_name + ".cxx"

out_file = open(out_file_name, "w")
if language == "java":
    package = get_json_item(fsm, "package")
    out_file.write("package " + package + ";\n")
elif language == "c/c++":
    headers = [
        "#include <stdio.h>\n",
        "#include <signal.h>\n",
        "#include <wchar.h>\n",
        "#include <sys/types.h>\n",
        "#include <sys/stat.h>\n",
        "#include <fcntl.h>\n",
        "#include \"rbc_base.h\"\n"
    ]
    out_file.writelines(headers)
out_file.write("\n")

# process path information
abs_target_path = get_json_item(fsm, "abs_target_path")
tool_chain_path = get_json_item(fsm, "tool_chain_path")

# write rbc_engine
rbc_engine = get_json_item(fsm, "rbc_engine")
if language == "java":
    out_file.write("import io.xc5.RBC_ENGINE;\n")
elif language == "c/c++":
    out_file.write("RBC_ENGINE " + rbc_engine + ";\n")
out_file.write("\n")

# write fsm function declare
if language == "java":
    out_file.write("public class " + out_file_name.split(".")[0] + " {\n")
    out_file.write("  ")
    out_file.write("public static void " + fsm_name + "() {\n")
elif language == "c/c++":
    out_file.write("int " + fsm_name + "_fsm(void)\n")
    out_file.write("{\n")

func_model_decl = rbc_engine + ".Model_decl("
func_build_begin = rbc_engine + ".Fsm_build_begin("
func_start_state = rbc_engine + ".Fsm_new_start_state("
func_final_state = rbc_engine + ".Fsm_new_final_state("
func_add_state = rbc_engine + ".Fsm_add_state("
func_add_transition = rbc_engine + ".Fsm_add_transition("
func_default_action = rbc_engine + ".Fsm_set_default_action("
func_build_end = rbc_engine + ".Fsm_build_end("

print_indent(out_file, language)
out_file.write(func_model_decl + func_build_begin + "\"" + fsm_name + "\"));\n")

fsm_start_state = get_json_item(fsm, "fsm_start_state")
print_indent(out_file, language)
out_file.write(func_model_decl + func_start_state + "\"" + fsm_start_state + "\"));\n")

fsm_final_state = get_json_item(fsm, "fsm_final_state")
print_indent(out_file, language)
out_file.write(func_model_decl + func_final_state + "\"" + fsm_final_state + "\"));\n")

fsm_add_states = fsm.get("fsm_add_states")
if fsm_add_states != None:
    for stat in fsm_add_states:
        name = stat.get("state")
        print_indent(out_file, language)
        out_file.write(func_model_decl + func_add_state + "\"" + name + "\"));\n")

fsm_transitions = get_json_item(fsm, "fsm_transitions")

for transit in fsm_transitions:
    in_state = transit.get("in_state")
    action = transit.get("action")
    demangle = subprocess.Popen(["c++filt", action], stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    fun_name = demangle.stdout.read()
    key = transit.get("key")
    if key == "":
        if language == "java":
            key = "null"
        elif language == "c/c++":
            key = "NULL"
    condition = transit.get("condition")
    out_state = transit.get("out_state")
    error_code = transit.get("error_code")
    if error_code == "":
        if language == "java":
            error_code = "\"\""
        elif language == "c/c++":
            error_code = "0"
    else:
        error_code = "\"" + error_code + "\""
    if language == "java":
        out_file.write("    // " + fun_name.decode())
        out_file.write("    ")
        out_file.write(func_model_decl + "\n")
        out_file.write("      " + func_add_transition + "\n")
        out_file.write("        \"" + in_state + "\", \"" + action + "\", " + key + ",\n")
        out_file.write("        " + condition + ", \"" + out_state + "\", " + error_code + "\n")
        out_file.write("        )\n")
        out_file.write("      );\n")
    elif language == "c/c++":
        out_file.write("  ")
        out_file.write(func_model_decl + func_add_transition + "\"" + in_state + "\", ")
        out_file.write("\"" + action + "\", ")
        out_file.write(key + ",\n")
        out_file.write("                                        ")
        out_file.write(condition + ",\n")
        out_file.write("                                        ")
        out_file.write("\"" + out_state + "\", ")
        out_file.write(error_code + "));\n\n")

fsm_default_action = fsm.get("fsm_default_action")
if fsm_default_action != None:
    for action in fsm_default_action:
        print_indent(out_file, language)
        out_file.write(func_model_decl + func_default_action + "\"" + action.get("name"))
        error_code = action.get("error_code")
        if error_code == "":
            if language == "java":
                error_code = "\"\""
            elif language == "c/c++":
                error_code = "0"
        else:
            error_code = "\"" + error_code + "\""
        out_file.write("\", " + error_code + "));\n")

print_indent(out_file, language)
out_file.write(func_model_decl + func_build_end + "\"" + fsm_name + "\"));\n")
if language == "java":
    out_file.write("  }\n")
out_file.write("}\n")
fsm_file.close()
out_file.close()

if arg_size != 3:
    exit(0)
if sys.argv[2] != "-archive":
    exit(0)
out_lib = "libfsm_" + fsm_name + ".a"
if language == "java":
    class_file = "FSM_" + fsm_name.upper() + ".class"
    obj_file = fsm_name + ".o"
    command_line = "javac -d . " + tool_chain_path + "/include/RBC_ENGINE.java"
    os.system(command_line)
    command_line = "javac -d . " + out_file_name
    os.system(command_line)
    source = ""
    for ss in package.split("."):
        source = source + ss + "/"
    source = source + class_file
    command_line = "xvsa -c -Wf,-RBC=true -INLINE:none " + source + " -o " + obj_file
    os.system(command_line)
    os.system("rm -rf sootOutput io " + package.split(".")[0])
elif language == "c/c++":
    obj_file = fsm_name + ".o"
    command_line = "xvsa -c " + out_file_name + " -o " + obj_file
    os.system(command_line)

command_line = "ar cru " + out_lib + " " + obj_file
os.system(command_line)
command_line = "mv " + out_lib + " " + abs_target_path + "/" + out_lib
os.system(command_line)
os.unlink(obj_file)
os.unlink(out_file_name)
