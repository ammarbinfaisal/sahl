from subprocess import Popen, PIPE
from os import environ
from sys import argv

files_all_checks = [
    "addition",
    "append",
    "cmpop",
    "control",
    "factorial",
    "fibonacci",
    "forin",
    "map",
    "mutate",
    "sample2",
    "sample3",
    "sample4",
    "sample",
    "simple",
    "sort",
    "store",
    "tuple",
    "while",
    "rule110",
    "bitwise",
    "cast",
]
files_retcode_check = [
    "chan",
    "neuralnet",
    "coroutine",
]

passing = 0
passing_aot = 0
passing_go = 0
total = len(files_all_checks) + len(files_retcode_check)

def passed(file):
    global passing
    passing += 1
    print(f"{file} passed")

def passed_go(file):
    global passing_go
    passing_go += 1
    print(f"{file} passed using go")

def run_file_byte(file, only_rc_check=False):
    print(f"running {file} in byte code")
    p = Popen(["./run_byte.sh", f"samples/{file}.sahl"], stdout=PIPE, stderr=PIPE)
    output, err = p.communicate(b"")
    rc = p.returncode
    if rc != 0:
        print(f"{file} failed")
        print(err.decode("utf-8") if len(err) > 0 else output.decode("utf-8"), end="")
        return
    if only_rc_check:
        passed(file)
        return
    output = output.decode("utf-8").split("\n\n\n\n\n")[1]
    with open(f"tests/{file}", "r") as f:
        expected = f.read()
    if expected == output:
        passed(file)
    else:
        print(f"{file} failed in bytecode")

def run_file_aot(file, only_rc_check=False):
    print(f"compiling {file} to x86_64")
    p = Popen(["./run_aot.sh", f"samples/{file}.sahl"], stdout=PIPE, stderr=PIPE)
    rc = p.wait()
    if rc != 0:
        print(f"couldn't compile {file}")
        return
    p = Popen(["./exe"], stdout=PIPE, stderr=PIPE)
    output, err = p.communicate(b"")
    rc = p.returncode
    if rc != 0:
        print(f"{file} failed in native")
        print(err.decode("utf-8") if len(err) > 0 else output.decode("utf-8"), end="")
        return
    global passing_aot
    passing_aot += 1

def run_file_go(file, only_rc_check=False):
    print(f"running {file} in go")
    p = Popen(["./run_go.sh", f"samples/{file}.sahl"], stdout=PIPE, stderr=PIPE)
    output, err = p.communicate(b"")
    rc = p.returncode
    if rc != 0:
        print(f"{file} failed in go")
        print(err.decode("utf-8") if len(err) > 0 else output.decode("utf-8"), end="")
        return
    if only_rc_check:
        passed_go(file)
        return
    output = output.decode("utf-8")
    with open(f"tests/{file}", "r") as f:
        expected = f.read()
    if expected == output:
        passed_go(file)
    else:
        print(f"{file} failed in go")

def run_all_go():
    for file in files_all_checks:
        run_file_go(file)

def run_all_byte():
    for file in files_all_checks:
        run_file_byte(file)

def run_all_aot():
    for file in files_all_checks:
        run_file_aot(file)

def go_result():
    print(f"{passing_go}/{total} go tests passed")

def aot_result():
    print(f"{passing_aot}/{total} aot tests passed")

def byte_result():
    print(f"{passing}/{total} bytecode tests passed")

if __name__ == "__main__":
    if len(argv) > 1:
        if argv[1] == "go":
            run_all_go()
            go_result()
        elif argv[1] == "aot":
            run_all_aot()
            aot_result()
        elif argv[1] == "byte":
            run_all_byte()
            byte_result()
        elif argv[1] in files_all_checks:
            run_file_byte(argv[1])
            run_file_aot(argv[1])
            run_file_go(argv[1])
        elif argv[1] in files_retcode_check:
            run_file_byte(argv[1], only_rc_check=True)
            run_file_aot(argv[1], only_rc_check=True)
            run_file_go(argv[1], only_rc_check=True)
        exit(0)
    run_all_byte()
    run_all_aot()
    run_all_go()
    byte_result()
    aot_result()
    go_result()
    if not "GITHUB_ENV" in environ:
        exit(0)
    with open(environ["GITHUB_ENV"], "w") as f:
        f.write(f"TEST_RESULT={passing}/{total}\n")
        f.write(f"COLOR={int((passing / total) * 100)}\n")
        f.write(f"AOT_TEST_RESULT={passing_aot}/{total}\n")
        f.write(f"AOT_COLOR={int((passing_aot / total) * 100)}\n")
        f.write(f"GO_TEST_RESULT={passing_go}/{total}\n")
        f.write(f"GO_COLOR={int((passing_go / total) * 100)}\n")
