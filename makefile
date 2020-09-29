PyGen: PyGen.hs
	stack ghc script.hs
	./script
	python3 script.py
clean:
	rm *.o
	rm ./script
	rm *.hi

