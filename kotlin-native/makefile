build-and-run:
	cinterop -def luamidi.def -o build/c_interop/luamidi && \
	konanc src/* -library build/c_interop/luamidi && \
	./program.kexe

clean:
	rm -rf build