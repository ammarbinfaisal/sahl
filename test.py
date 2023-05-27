from subprocess import Popen, PIPE
from os import environ

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
]
files_retcode_check = [
    "chan",
    "neuralnet",
    "rule110",
    "coroutine",
]

passing = 0
total = len(files_all_checks) + len(files_retcode_check)

def passed(file):
    global passing
    passing += 1
    print(f"{file} passed")

def run_file(file, only_rc_check=False):
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
        print(f"{file} failed")
        print(f"Expected: {expected}")
        print(f"Got: {output}")


if __name__ == "__main__":
    for file in files_all_checks:
        run_file(file)
    for file in files_retcode_check:
        run_file(file, only_rc_check=True)
    result = f"{passing}/{total}"
    print(f"{result} tests passed")
    if not "GITHUB_ENV" in environ:
        exit(0)
    with open(environ["GITHUB_ENV"], "w") as f:
        f.write(f"TEST_RESULT={result}\n")
        f.write(f"COLOR={int((passing / total) * 100)}\n")
