PyGen: PyGen.hs
	stack ghc PyGen.hs
	./PyGen
clean:
	rm *.o
	rm PyGen
	rm *.hi

