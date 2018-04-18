build-and-run:
	cinterop -def stdio.def -o build/c_interop/hello && \
	konanc src/* -library build/c_interop/hello -o hello && \
	mv hello.kexe hello && ./hello

clean:
	rm -rf build