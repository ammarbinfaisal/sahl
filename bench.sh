# make all

echo "Adding from 1 to 1000000"

./target/release/sahl "samples/addition.sahl" -c
hyperfine "python3 samples/addition.py" "./target/release/sahl samples/addition.sahl -e" "./sahl exe.bin"


echo "Appending 0 to 1000000 to a list"

./target/release/sahl "samples/append.sahl" -c
hyperfine "python3 samples/append.py" "./sahl exe.bin"


echo "Fibonacci"

hyperfine "python3 samples/fibonacci.py" "./target/release/sahl samples/fibonacci.sahl -e"


echo "Factorial of 20"

hyperfine "python3 samples/factorial.py" "./target/release/sahl samples/factorial.sahl -c"


echo "Channels"

hyperfine "yaegi samples/chan.go" "./target/release/sahl "samples/chan.sahl" -e"
