//Left off for a timeout for read()
//3 quarters downm
/*** INCLUDES ***/
#include <errno.h> //Error codes
#include <ctype.h> //for Ctrl vals
#include <stdio.h> //printf() and perror() 
#include <stdlib.h> //for exit and atexit()
#include <string.h> //string used for realloc() and memcpy()
#include <unistd.h> //for read(), STDIN_FILENO
#include <sys/ioctl.h>//Getting the window size the easy way IOCtl (which itself stands for Input/Output Control) Get WINdow SiZe.)
#include <termios.h> //for manipulating terminal I/O settings
//note a structure is a user defined data type that is used to group items of different types into a single type
//almost like object I guess (java king)
 
/*** DEFINES ***/
#define CTRL_KEY(k) ((k) & 0x1f)
#define DOCKERKILO_VERSION "0.0.1"

enum editorKey {
  	ARROW_LEFT = 1000, //setting first constant to 1000, subsequent constants get incremental values
  	ARROW_RIGHT, //I.E 1001,1002
  	ARROW_UP,
  	ARROW_DOWN,
  	PAGE_UP,
  	PAGE_DOWN
};

/*** DATA ***/
struct editorConfig {
	int cx, cy;//Moving that cursor is crazy work lmao
	int screenrows;
	int screencols;
  	struct termios orig_termios;
};
struct editorConfig E; //Think like an object E that contains the termios and orig_termios variables within it
//termios from termios, used to store terminal settings, very important
//Orig_termios is used for storing original data so we can save on exit of the program

/*** TERMINAL  ***/
void die(const char *s){
	//Clear the screen on exit
	write(STDOUT_FILENO, "\x1b[2J", 4);
  	write(STDOUT_FILENO, "\x1b[H", 3);

	//method handles fatal errors prints string s on exit
	perror(s);
	exit(1);
}
void disableRawMode(){
	//disabling the raw mode by setting attributes to the original terminal
	//STDIN_FILENO is a file descriptor for standard input
	//TCSAFLUSH apply changes after all pending output is written and discard unread input
	//&orig_termios is a pointer to the saved original settings (C pointers FTW)
	if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1)
    die("tcsetattr");
}
int editorReadKey() {
	//pretty self explanitory, read the input key on the terminal for now
  	int nread;
  	char c;
  	while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
    	if (nread == -1 && errno != EAGAIN) die("read");
  	}
  	return c;
	/*pressing an arrow key sends multiple bytes as input to our program. 
	 These bytes are in the form of an escape sequence
	 that starts with '\x1b', '[', followed by an 'A', 'B', 'C', or 'D' 
	 depending on which of the four arrow keys was pressed.*/
  	
	
	//If we read an escape character,we read two more bytes into seq immediately.
	/*otherwise we look to see if the escape sequence is an arrow key escape sequence
	if it is we just return the corresponding wasd character.*/
	if (c == '\x1b') {
    	char seq[3];//Note seq is only 3 bytes as we want longer escape sequences in the future
    	if (read(STDIN_FILENO, &seq[0], 1) != 1) return '\x1b';
    	if (read(STDIN_FILENO, &seq[1], 1) != 1) return '\x1b';
    	if (seq[0] == '[') {
			if(seq[1] >= '0' && seq[9] <= '9'){
				//You must go on from here.//****************************************************************************************************** */
			}
			switch (seq[1]) {
				case 'A': return ARROW_UP;
				case 'B': return ARROW_DOWN;
				case 'C': return ARROW_RIGHT;
				case 'D': return ARROW_LEFT;
			}
    	}
    	return '\x1b';
  	} else {
    	return c;
  	}
}
void enableRawMode() {
	//tcgetattr gets the terminal attributes from file descriptor and stores in orig_termios pointer
    tcgetattr(STDIN_FILENO, &E.orig_termios);//set the terminal attributes

	//use this to register our disableRawMode() function to be called auto when program exit
	//leave it the way we found it. even if we are exiting on a CTRL_C
	atexit(disableRawMode);
    
	//Store the original terminal attributes in global var (making copy before changing everything)
	struct termios raw = E.orig_termios;

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
int getCursorPosition(int *rows, int *cols){
	char buf[32];//our buffer
	unsigned int i = 0;
	if(write(STDOUT_FILENO, "\x1b[6n",4) != 4) return -1;
		while (i < sizeof(buf) - 1) {
	    if (read(STDIN_FILENO, &buf[i], 1) != 1) break;
	    if (buf[i] == 'R') break;
    	i++;
  	}
  	buf[i] = '\0';
  	if (buf[0] != '\x1b' || buf[1] != '[') return -1;
  	if (sscanf(&buf[2], "%d;%d", rows, cols) != 2) return -1;
  	return 0;
}
int getWindowSize(int *rows, int *cols) {
  	struct winsize ws; //comes from <sys/ioctl.h>
	//IOCtl (which itself stands for Input/Output Control) Get WINdow SiZe.)

	//Basically, get the window size using the function, if this function fails, we will return -1;
	//If the function succeeds it plays the # rows/columns into the winsize construct lololol (I'm going insane a little )
  	if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {

		/*As you can see, there is no easy way to get the cursor the bottom of the screen
		  We are just sending the cursor there by using a large value of 999 to ensure it reaches teh bottom right of the screen!*/
		if(write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) return -1;
		/*The C and B commands are documented to stop the cursor from going past the edge of the screen*/
    	return getCursorPosition(rows,cols); 
  	} else {
    	*cols = ws.ws_col;
    	*rows = ws.ws_row;
    	return 0;
  	}
}

/*** APPEND BUFFER ***/
//This is really annoying c jargen and these methods are used to allocate enough memory to hold the new string.
struct abuf {
	char *b; //pointer to buffer in memory
	int len;
};
#define ABUF_INIT {NULL, 0} // ACTING as constructor from our abuf type

void abAppend(struct abuf *ab, const char *s, int len){
	//To append a new string s onto abuf. we need to use realloc
	char *new = realloc(ab -> b, ab ->len + len);//comes from string. gives us a block of memory to work with
												 //so we can append the new string onto the previous string.
	if(new == NULL) return ;

	//memcpy copies the string s after the end of the current data  in the buffer.
	//update the pointer and length of abuf to the new values. 
	// In hindsight, I'm very glad C gives us the ability to allocate memory 
	// to append new data onto strings (Quite cool)
	memcpy(&new[ab->len], s, len);
	ab->b = new;
	ab->len += len;
}
//Deconstructor that deallocate thes dynamic memory used by an abuf
void abFree(struct abuf *ab){
	free(ab->b);
}
/*** OUTPUT ***/
void editorDrawRows(struct abuf *ab) {
  int y;
  for (y = 0; y < E.screenrows; y++) {
    if (y == E.screenrows / 3) {//Printing our welcome message to the screen.
      char welcome[80];
      int welcomelen = snprintf(welcome, sizeof(welcome),
        "DockerKilo editor -- version %s", DOCKERKILO_VERSION);
      if (welcomelen > E.screencols) welcomelen = E.screencols;
	  int padding = (E.screencols - welcomelen) /2; //Centering the text
	  if(padding){
		abAppend(ab,"~",1);
		padding--;
	  }
	  while(padding--) abAppend(ab, " ",1 );
      abAppend(ab, welcome, welcomelen);
    } else {
      abAppend(ab, "~", 1);
    }
    abAppend(ab, "\x1b[K", 3);
    if (y < E.screenrows - 1) {
      abAppend(ab, "\r\n", 2);
    }
  }
}
void editorRefreshScreen(){
	//using our string buffer to append data to the screen
	struct abuf ab = ABUF_INIT;

	abAppend(&ab, "\x1b[?25l",6);//possible cursor might be displayed in the middle of the screen somewhere for a split second.
  	abAppend(&ab, "\x1b[H", 3);//Appends the escape sequence to move the cursor to the top-left corner ([H means "move to row 1, column 1").

  	editorDrawRows(&ab);
	
 	char buf[32];
    snprintf(buf, sizeof(buf), "\x1b[%d;%dH", E.cy + 1, E.cx + 1);
  	abAppend(&ab, buf, strlen(buf));


	abAppend(&ab, "\x1b[H", 3);//Again, moves the cursor back to the top-left corner after drawing rows.
  	abAppend(&ab, "\x1b[?25h",6);//We use escape sequences to tell the terminal to hide and show the cursor.
	
	write(STDOUT_FILENO, ab.b, ab.len);
  	abFree(&ab);
}

/*** INPUT ***/
void editorMoveCursor(int key) {
  switch (key) {
    case ARROW_LEFT:
      if (E.cx != 0) {
        E.cx--;
      }
      break;
    case ARROW_RIGHT:
      if (E.cx != E.screencols - 1) {
        E.cx++;
      }
      break;
    case ARROW_UP:
      if (E.cy != 0) {
        E.cy--;
      }
      break;
    case ARROW_DOWN:
      if (E.cy != E.screenrows - 1) {
        E.cy++;
      }
      break;
  }
}
void editorProcessKeypress() {
  	int c = editorReadKey();
  	switch (c) {
    	case CTRL_KEY('q'):
		//Clearing the screen, reposition the cursor
	  		write(STDOUT_FILENO, "\x1b[2J", 4);
      		write(STDOUT_FILENO, "\x1b[H", 3);
	        exit(0);
      		break;
		case 'q':
	  		write(STDOUT_FILENO, "\x1b[2J", 4);
      		write(STDOUT_FILENO, "\x1b[H", 3);
		    exit(0);
	  		break;
	  
    	case ARROW_UP:
    	case ARROW_DOWN:
    	case ARROW_LEFT:
    	case ARROW_RIGHT:
      		editorMoveCursor(c);
      		break;
	}
}

/*** INIT ***/
void initEditor(){
	E.cx = 0;
	E.cy = 0;
	if(getWindowSize(&E.screenrows, &E.screencols) == -1) die("Get Windows Size");
}
int main(){
	enableRawMode();
	initEditor();//Initializing the editor to get the terminal screenrows/cols the EASY way. (why make this difficult)
	//asking to read 1 byte from the standard input into the var c, returns 0 when no more bytes left to read
	//Terminal will always start in cooked mode(keyboard input is only sent to your program when the user presses enter)
	//We want raw mode where the user keypresses are inputting as they are typed.
	while(1){
		editorProcessKeypress();
		editorRefreshScreen();
		
	}

	return 0;
}
