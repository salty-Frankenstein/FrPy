script: script.hs
	stack ghc script.hs
	./script
	python3 script.py
clean:
	find . -name "*.o" | xargs rm -f
	find . -name "*.hi" | xargs rm -f
	rm -f ./script

