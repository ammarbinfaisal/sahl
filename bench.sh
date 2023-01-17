# make all

echo "Adding from 1 to 1000000"

./target/release/sahl "samples/addition.sahl" -c
hyperfine "python3 samples/addition.py" "./target/release/sahl samples/addition.sahl -e" "./sahl exe.bin"


echo "Appending 0 to 1000000 to a list"

./target/release/sahl "samples/append.sahl" -c
hyperfine "python3 samples/append.py" "./sahl exe.bin"


echo "Fibonacci"

./target/release/sahl "samples/fibonacci.sahl" -c
hyperfine "python3 samples/fibonacci.py" "./sahl exe.bin"


echo "Factorial of 20"

./target/release/sahl "samples/factorial.sahl" -c
hyperfine "python3 samples/factorial.py" "./sahl exe.bin"


echo "Channels"

hyperfine "yaegi samples/chan.go" "./target/release/sahl "samples/chan.sahl" -e"
