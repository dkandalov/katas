build-and-run:
	cinterop -def stdio.def -o build/c_interop/stdio && \
	konanc src/* -library build/c_interop/stdio && \
	./program.kexe