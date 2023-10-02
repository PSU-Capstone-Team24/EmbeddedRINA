#include <stdio.h>
#include <unistd.h> // read(), write(), close()
#include <strings.h> // bzero()

void doFlow(int fDesc){
	char buff[80];

	for(;;){
		read(fDesc, buff, sizeof(buff));
		printf("%s\n", buff);
		bzero(buff, 80);
		fflush(stdout);
	}
}