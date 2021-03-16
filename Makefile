all: clean
	ros run --eval "(ql:quickload '(:trivial-gamekit/distribution :multi-test))" --eval "(gamekit.distribution:deliver :multi-test 'multi-test::multiplayer :build-directory \"build/\")" --eval "(quit)"

clean:
	rm -rf build
