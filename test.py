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

passing = {
    "byte": 0,
    "aot": 0,
    "go": 0,
}
total = len(files_all_checks) + len(files_retcode_check)


def passed(file, mode):
    global passing
    passing[mode] += 1
    print(f"{file} passed")

def failed(file, mode):
    print(f"{file} failed in {mode}")

def run_file(mode, file, only_rc_check=False):
    print(f"running {file} in {mode}")
    p = Popen([f"./run_{mode}.sh", f"samples/{file}.sahl"], stdout=PIPE, stderr=PIPE)
    output, err = p.communicate(b"")
    rc = p.returncode
    if rc != 0:
        failed(file, mode)
        return
    if only_rc_check or mode == "aot":
        passed(file, mode)
        return
    output = (
        output.decode("utf-8").split("\n\n\n\n\n")[1]
        if mode == "byte"
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

modes = ["byte", "aot", "go"]

if __name__ == "__main__":
    if len(argv) > 1:
        if argv[1] in modes:
            run_all(argv[1])
            result(argv[1])
        elif argv[1] in files_all_checks:
            run_file("byte", argv[1])
            run_file("aot", argv[1])
            run_file("go", argv[1])
        elif argv[1] in files_retcode_check:
            run_file("byte", argv[1], True)
            run_file("aot", argv[1], True)
            run_file("go", argv[1], True)
        exit(0)
    for mode in ["byte", "aot", "go"]:
        run_all(mode)
    for mode in ["byte", "aot", "go"]:
        result(mode)
    if not "GITHUB_ENV" in environ:
        exit(0)
    with open(environ["GITHUB_ENV"], "w") as f:
        f.write(f"TEST_RESULT={passing['byte']}/{total}\n")
        f.write(f"COLOR={int((passing['byte'] / total) * 100)}\n")
        f.write(f"AOT_TEST_RESULT={passing['aot']}/{total}\n")
        f.write(f"AOT_COLOR={int((passing['aot'] / total) * 100)}\n")
        f.write(f"GO_TEST_RESULT={passing['go']}/{total}\n")
        f.write(f"GO_COLOR={int((passing['go'] / total) * 100)}\n")
