//Left off for a timeout for read()
//3 quarters downm
/*** INCLUDES ***/
#include <errno.h> //Error codes
#include <ctype.h> //for Ctrl vals
#include <stdio.h> //printf() and perror() 
#include <stdlib.h> //for exit and atexit()
#include <unistd.h> //for read(), STDIN_FILENO
#include <termios.h> //for manipulating terminal I/O settings
//note a structure is a user defined data type that is used to group items of different types into a single type
//almost like object I guess (java king)
 
#define CTRL_KEY(k) ((k) & 0x1f)

/*** DATA ***/
//termios from termios, used to store terminal settings, very important
//Orig_termios is used for storing original data so we can save on exit of the program
struct termios orig_termios;

/*** TERMINAL  ***/
void die(const char *s){
	//method handles fatal errors prints string s on exit
	perror(s);
	exit(1);
}

void disableRawMode(){
	//disabling the raw mode by setting attributes to the original terminal
	//STDIN_FILENO is a file descriptor for standard input
	//TCSAFLUSH apply changes after all pending output is written and discard unread input
	//&orig_termios is a pointer to the saved original settings (C pointers FTW)
	if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios) == -1)
    die("tcsetattr");
}

void enableRawMode() {
	//tcgetattr gets the terminal attributes from file descriptor and stores in orig_termios pointer
    tcgetattr(STDIN_FILENO, &orig_termios);//set the terminal attributes

	//use this to register our disableRawMode() function to be called auto when program exit
	//leave it the way we found it. even if we are exiting on a CTRL_C
	atexit(disableRawMode);
    
	//Store the original terminal attributes in global var (making copy before changing everything)
	struct termios raw = orig_termios;

	//Note c_iflag is ctrl input flag which records which keypress is pressed lol
	//disable ctrl-s and ctrl-q which freeze/unfreeze our beut of a program
	//While pressing CTRL, we can type everything but m, which is being read as 10 bytes (not 13 like we expect)
	//Because CTRL-J already produces 10, and the enter key does aswell, ICRNL correct so ctrl-m == 13
	/*BRKINT: prevents break condition from causing SIGINT.

	ICRNL: disables conversion of carriage return (\r) to newline (\n).

	INPCK: disables parity check (used for serial).

	ISTRIP: disables stripping 8th bit.

	IXON: disables software flow control (Ctrl-S/Ctrl-Q).*/
  	raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
  	raw.c_oflag &= ~(OPOST);//turn off output processing
	raw.c_cflag |= (CS8);//misc flag
	//c_lflag is for local flags, ICANON allows us to turn off canonical mode (READING BYTE TO BYTE FINALLY not line by line)
	//The Icanon flag will let us exit on pressing of q, not when we press enter
	//The Isig flag lets us read ctrl-c and ctrl-z
	//IEXTEN disable ctrl-v
	raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);//modifying the struct by hand
  	
	//vmin/vtim both from termios, indexes into the c_cc field which are control characters
	//Vmin sets minimum # of bytes of input needed before read() can return (set to 0 to read() returns as soon as there is any input to be read)
	//VTIME value sets max amount time to wait before read() returns
	raw.c_cc[VMIN] = 0;
	raw.c_cc[VTIME] = 1;

	//TCSAFLUSH tells when to apply the change, waiting for the pending output to be written to terminal (discarding any undread input)
	tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);//passing the modified struct bake into the method to write the new terminal attributes back out
    //struct termios, tcgetattr(), tcsetattr(), ECHO, and TCSAFLUSH all come from <termios.h>
	//this function applies all the settings we applied to the terminal 
	
	/*The Echo feature causes each key you type to be printed to the terminal so you can see what your typing
	  Very useful in canonical mode but gets in the way in raw mode so we turn it off*/  
	if(tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == 1) die("tcsetattr");
}

/*** OUTPUT ***/
void editorRefreshScreen(){
	write(STDOUT_FILENO, "\x1b[2j",4);
}


/*** input ***/
char editorReadKey() {
  int nread;
  char c;
  while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
    if (nread == -1 && errno != EAGAIN) die("read");
  }
  return c;
}
void editorProcessKeypress() {
  char c = editorReadKey();
  switch (c) {
    case CTRL_KEY('q'):
      exit(0);
      break;
	case 'q':
	  exit(0);
	  break;	  
	}
}

/*** INIT ***/
int main(){
	enableRawMode();
	//asking to read 1 byte from the standard input into the var c, returns 0 when no more bytes left to read
	//Terminal will always start in cooked mode(keyboard input is only sent to your program when the user presses enter)
	//We want raw mode where the user keypresses are inputting as they are typed.
	while(1){
		editorProcessKeypress();
		editorRefreshScreen();
	}

	return 0;
}
