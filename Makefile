all: cm.exe

cm.exe:
	gcc cm.c -o cm.exe

clean:
	rm -f *.o cm.exe
