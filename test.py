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

byte_mode = 0
aot_mode = 1
go_mode = 2

passing = {
    byte_mode: 0,
    aot_mode: 0,
    go_mode: 0,
}
total = len(files_all_checks) + len(files_retcode_check)

mode_to_str = {
    byte_mode: "byte",
    aot_mode: "aot",
    go_mode: "go",
}

def passed(file, mode):
    global passing
    passing[mode] += 1
    print(f"{file} passed")

def failed(file, mode):
    print(f"{file} failed in {mode_to_str[mode]}")

def run_file(mode, file, only_rc_check=False):
    modestr = mode_to_str[mode]
    print(f"running {file} in {modestr}")
    p = Popen([f"./run_{modestr}.sh", f"samples/{file}.sahl"], stdout=PIPE, stderr=PIPE)
    output, err = p.communicate(b"")
    rc = p.returncode
    if rc != 0:
        failed(file, mode)
        return
    if only_rc_check or mode == aot_mode:
        passed(file, mode)
        return
    output = (
        output.decode("utf-8").split("\n\n\n\n\n")[1]
        if mode == byte_mode
        else output.decode("utf-8")
    )
    with open(f"tests/{file}", "r") as f:
        expected = f.read()
    if expected == output:
        passed(file, mode)
    else:
        failed(file, mode)


def run_all(mode):
    for file in files_all_checks:
        run_file(mode, file)


def result(mode):
    print(f"{passing[mode]}/{total} {mode} tests passed")

modes = [byte_mode, aot_mode, go_mode]

if __name__ == "__main__":
    if len(argv) > 1:
        if argv[1] in modes:
            run_all(argv[1])
            result(argv[1])
        elif argv[1] in files_all_checks:
            run_file(byte_mode, argv[1])
            run_file(aot_mode, argv[1])
            run_file(go_mode, argv[1])
        elif argv[1] in files_retcode_check:
            run_file(byte_mode, argv[1], True)
            run_file(aot_mode, argv[1], True)
            run_file(go_mode, argv[1], True)
        exit(0)
    for mode in [byte_mode, aot_mode, go_mode]:
        run_all(mode)
    for mode in [byte_mode, aot_mode, go_mode]:
        result(mode)
    if not "GITHUB_ENV" in environ:
        exit(0)
    with open(environ["GITHUB_ENV"], "w") as f:
        f.write(f"TEST_RESULT={passing[byte_mode]}/{total}\n")
        f.write(f"COLOR={int((passing[byte_mode] / total) * 100)}\n")
        f.write(f"AOT_TEST_RESULT={passing[aot_mode]}/{total}\n")
        f.write(f"AOT_COLOR={int((passing[aot_mode] / total) * 100)}\n")
        f.write(f"GO_TEST_RESULT={passing[go_mode]}/{total}\n")
        f.write(f"GO_COLOR={int((passing[go_mode] / total) * 100)}\n")
