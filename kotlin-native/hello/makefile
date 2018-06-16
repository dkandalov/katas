build-and-run:
	cinterop -def stdio.def -o build/c_interop/stdio && \
	konanc src/* -library build/c_interop/stdio -o hello && \
	mv hello.kexe hello && ./hello

clean:
	rm -rf build