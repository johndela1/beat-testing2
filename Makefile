_ffi_ex: termops.o
	ghc --make -main-is FfiEx -o ffi_ex FfiEx.hs termops.o
clean:
	rm *.o *.hi a.out
