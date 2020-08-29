; A small tool to eject, close_track, lock, unlock or reset
; any CD-ROM, selected by the drive letter. Freeware.
;
; If you want to use my source code in another project, please
; ask me for permission first! Including in GNU software is
; always allowed as long as you keep this source code protected
; by the http://www.gnu.org GPLv2 license as well. Written and
; conceived in 2003 by Eric Auer, <eric * coli.uni-sb.de>.
;
; To compile (http://nasm.sf.net/): nasm -o cdrom.com cdrom.asm
; Usage syntax examples:
; CDROM LOCK X:
; CDROM EJECT Y:
; ... you get it. Keywords: LOCK UNLOCK EJECT CLOSE RESET
;
; New 11.11.2003: If you specify a non-CD-ROM drive letter,
; CDROM will show you a list of all valid CD-ROM drive letters.
; Program still is < 1024 bytes in size :-).
;
; New 16.11.2003: If you compile with -DCDDA, you will be able
; to play audio CDs with this program, too. New commands:
; PAUSE, CONTINUE, INFO, PLAYxx (xx is a track number < 100)
; Compile that version: nasm -DCDDA -o cdrom.com cdrom.asm

; int 2f ax=150c bx=0 -> returns major version BH, minor BL.
; int 2f ax=1510 cx=driveletter (0 based) es:bx->request header
; int 2f ax=1500 bx=0 -> returns number of CD-ROM drive
;   letters in BX, first drive number in CX (0 based).
; int 2f ax=150d es:bx=pointer to array -> returns array of
;   drive numbers (0 based, 1 byte for each drive letter).
;   (CD-ROM 2.0+, older support pretty different ax=1501)
;
; Request header: byte length, byte subunit (filled in by *CDEX)
;   byte command (0x0c for IOCTL write), word returned status
;   (filled in by driver), 8 bytes reserved, byte 0xf8
;   (media descriptor, offset 0x0d), dword pointer to DATA
;   word size of DATA. DATA = 1 or 2 bytes (function, option)
;
; IOCTL: 0 = eject 5 = close 2 = reset 1.0 = unlock 1.1 = lock.
;
; Audio: Command 0x85 pause 0x88 continue
;     (request header only 0x0d long, no special fields)
;   Command 0x84 play (from offset 0x0d request header on:
;     byte 0 HSG (1 RB, if supported), dword start-time,
;     dword sector count. (start-time in HSG or RB)
; HSG address = (((60*m)+s)*75)+150, RB address = f,s,m,0
; IOCTL READ: Request header as with IOCTL WRITE, but command
;   is 3, not 0x0c, and DATA is byte type-of-data (on call),
;   followed by returned data, as follows:
;   (type 6 - returns dword drive flags. Bits: 0 door open,
;     1 door unlocked, 2 can rawread, 3 can write, 4 can play
;     audio/video, 5 can interleave, 7 can prefetch, 9 does
;     allow RB addr., 10 is playing audio, 11 no disk in drive)
;   (type 8 - returns dword disk size in sectors)
;   (type 9 - byte change flag: 0 unknown, 1 changed, -1 unch.)
;   type 0x0a - returns byte mintrack, byte maxtrack, dword
;     end of audio tracks (RB address)
;   type 0x0b - byte track number (1-99, set by caller!),
;     dword starting point a RB address, byte/word track flags 

	org 0x100	; it is a com file

; -----------------------------

start:  mov al,0x0d	; a 0x0d byte: 4DOS fails to terminate
	cld		; the command line string if > 126 char
	mov al,-1
	mov di,drives	; the "possible drive letter list"
	mov cx,32
	rep stosb	; zap the list (init BSS)

	mov si,0x81	; command line arguments
cparse:	mov al,[si]	; get a char
	cmp al,0x0d	; end of line? then we are done.
	jz cdone
	cmp al,':'	; after a drive letter? then read it.
	jz knodrv
uknown:
%ifdef CDDA
	cmp al,'a'	; upcasing needed?
	jb known
%endif
	and al,0xdf	; a-z -> A-Z (trashes non-alphabetics)
	mov [si],al	; store upcased version
known:	inc si		; continue parsing
	jmp short cparse

knodrv:	mov al,[si-1]
	and al,0xdf	; a-z -> A-Z ((or trash if non-alpha))
	sub al,'A'	; A: -> 0 etc.
	cmp al,26	; drive number in 0..25 range?
	jae uknown	; else ignore
	mov [seldrv],al	; store selected drive number!
	jmp short known

cdone:	mov al,[ds:seldrv]	; which drive selected?
	cmp al,-1	; if none at all, complain
	jnz gowork	; else continue processing
	mov dx,helpmsg
	jmp short failed

; -----------------------------

failed:	mov ah,9	; show string
	int 0x21	; DOS API
	mov ax,0x4c01	; return with errorlevel 1
	int 0x21	; DOS API

; -----------------------------

gowork:	mov ax,0x1500	; CD-ROM install check
	mov bx,0	; if no driver at all
	int 0x2f	; multiplex interrupt
	mov dx,nocdrom	; error message: no drives!
	mov [mindrv],cl	; store first CD-ROM drive number
	mov [ndrives],bl	; number of drives
	or bx,bx	; any drives at all?
	jz failed

anycd:	mov ax,0x150c	; CD-ROM version check
	mov bx,0	; if no 2.0+ version
	int 0x2f	; MUX
	cmp bh,2
	jae newcd

oldcd:	mov al,[ds:ndrives]
	dec al
	add [cdx1end],al	; last is X after first
	mov al,[ds:mindrv]
	add [cdx1beg],al	; first is X after A:
	add [cdx1end],al	; last is X+... after A:
	mov dx,cdx1msg	; warning: old *CDEX version
	mov ah,9
	int 0x21	; DOS API
	or byte [ndrives],0x80	; flag for "old *CDEX"
	jmp short old2

newcd:  mov ax,0x150d	; get array of drive letters
	mov bx,drives	; destination array (in ES)
	int 0x2f	; MUX

old2:	mov al,[ds:seldrv]	; drive to be IOCTLed
	cmp al,[ds:mindrv]	; first valid drive
	jb invdrv	; invalid drive selected
	xor cx,cx
	mov cl,[ds:ndrives]	; drive letter count
	or cl,cl	; any drives?
	jz exdrv	; if not, no drives to be checked.
	js exdrv1	; flag set, *CDEX < 2.0, no check
	mov bx,drives	; drive letter array
chkdrv:	cmp al,[bx]
	jz exdrv
	inc bx
	loop chkdrv	; repeat until found

invdrv:	mov al,[ds:seldrv]	; the user-selected drive letter
	add [nocddrv],al	; add it to the error message
	mov dx,nocddrv	; error message: no CD-ROM letter
	mov ah,9	; NEW: show list of existing drives!
	int 0x21	; DOS API
	mov bx,drives	; drive letter array
	xor cx,cx
	mov cl,[ds:ndrives]
shdrv:	mov al,[bx]
	cmp al,25	; in A..Z range?
	ja shdrv2	; else do not display
	mov ah,2	; write char to STDOUT (CON)
	mov dl,'A'	; the char to be written
	add dl,al	; translate drive number to char
	int 0x21	; DOS API
	mov ah,9	; string output
	mov dx,colon	; add ": "
	int 0x21	; DOS API
shdrv2:	inc bx		; next list entry
	loop shdrv	; loop over array
	mov dx,nocdONE	; message end for "1" case
	mov al,[ds:ndrives]
	cmp al,1
	jz shdrv3
	mov dx,nocdN	; message end for ">1" case
shdrv3:	jmp failed	; leave with message and errorlevel 1

; -----------------------------

exdrv1:
exdrv:			; parse: which command is requested?
	mov si,cmds	; command list
wordlp:	mov bx,[si]	; pointer to the command name
	or bx,bx	; end of list?
	jz done		; no command requested at all (okay!)
	mov cx,[bx]	; first 2 chars of the name
	mov di,0x81	; command line
wordl2:	mov ax,[di]	; is it THERE ?
	cmp ax,cx
	jz dothat
	cmp al,0x0d	; end of line?
	jz wordl3	; then try again with next command
	inc di		; parse on
	jmp short wordl2

wordl3:	add si,4	; next command table entry
	jmp short wordlp	; search that one next

; -----------------------------

dothat:	add si,2	; -> command FUNCTION pointer
	mov si,[si]	; fetch that pointer
	mov ah,9	; string output
	mov dx,cmdmsg	; confirmation message
	int 0x21	; DOS API
	mov dx,bx	; command name
	mov ah,9	; string output
	int 0x21	; DOS API
	mov dx,crlfmsg	; line break
	mov ah,9	; string output
	int 0x21	; DOS API

	mov ax,ds	; segment of IOCTL data
	mov [datS],ax	; fill in in reqest header

	call si		; *** call that FUNCTION ***

	jnc done	; everything okay?
	mov dx,failmsg	; error message: function failed
	jmp failed

done:	mov ah,9	; string output
	mov dx,donemsg	; message: done
	int 0x21	; DOS API
	mov ax,0x4c00	; return errorlevel 0
	int 0x21	; DOS API

; -----------------------------

ejectC:	mov ax,0	; 0: eject
%ifdef CDDA
	mov byte [dsize],1	; command only
%endif
	jmp short IOCTLW

closeC:	mov ax,5	; 5: close tray
%ifdef CDDA
	mov byte [dsize],1	; command only
%endif
	jmp short IOCTLW

lockC:	mov byte [dsize],2	; command plus argument
	mov ax,0x0101	; 1.1: lock CD-ROM in drive
	jmp short IOCTLW
	
ulockC:	mov byte [dsize],2	; command plus argument
	mov ax,0x0001	; 1.0 (sic!): unlock CD-ROM in drive
	jmp short IOCTLW

resetC:	mov ax,2	; 2: reset
%ifdef CDDA
	mov byte [dsize],1	; command only
%endif
	jmp short IOCTLW

; -----------------------------

%ifdef CDDA

IOCTLW:	jmp IOCTLW2

pauseC:	mov byte [rcmd],0x85	; PAUSE audio
	mov byte [rlen],0x0d	; request header length
	jmp CDRDEV

contC:	mov byte [rcmd],0x88	; CONTINUE / resume audio
	mov byte [rlen],0x0d	; request header length
	jmp CDRDEV

infoC:	mov byte [playbin],-1	; play no track, just show info
	jmp cddaPlayInfo

playEE:	mov dx,playsyntaxmsg	; complain: must use PLAYxx.
	jmp failed

	; PLAY an AUDIO TRACK
playC:	add di,2		; make DI point at "AY" ("PLAY")
	mov ax,[di]		; fetch "AY" or track number
	cmp ax,[ds:playW+2]	; is it "AY" at all?
	jnz play2		; else, assume short "PLxx" form
	inc di
	inc di			; long "PLAYxx" form
	mov ax,[di]		; fetch ASCII track number string
play2:	sub ax,"00"		; ASCII to binary
	cmp al,9		; initial digit?
	ja playEE		; error: no initial digit
	cmp ah,9		; second digit?
	jbe play3		; 2 digits found
	mov ah,al		; make "second" digit...
	mov al,0		; make initial "0"
play3:	push ax
	add ax,"00"		; make ASCII again
	mov [playasc],ax	; store for string display
	pop ax
	xchg al,ah		; put 2nd, lower, digit into AL
				; and 1st, higher, digit into AH
	aad			; AX = (10*AH) + AL
	mov [playbin],al	; store for playing
	;
	mov dx,playmsg		; Message about playing audio
	mov ah,9		; string output
	int 0x21		; DOS API
	;
cddaPlayInfo:
	mov byte [dsize],1+4	; returns a dword...
	mov byte [thedata],6	; data type: drive flags
	call IOCTLR		; IOCTL READ
	jc cpitry		; on error, skip flags test
	mov ax,[thedata+1]	; low word of drive flags
	test al,1		; door open?
	jz cpi1
	mov dx,doormsg		; complain about open ddrive door
	jmp failed

cpi1:	test al,8		; is it even a CD/DVD WRITER?
	jz cpi2
	push ax
	mov dx,cdwrmsg		; tell that this is a WRITER
	mov ah,9		; string output
	int 0x21		; DOS API
	pop ax
cpi2:	test al,0x10		; can it play audio?
	jnz cpi3
	mov dx,nocddamsg	; complain about lacking audio
	jmp failed

cpi3:	test ah,4		; currently playing audio?
	jz cpi4
	push ax
	cmp byte [playbin],-1	; are we trying to play at all?
	jz cpi3i		; else no pause needed!
	mov dx,cdbusymsg	; tell that audio is playing: might
		; only accept a new PLAY command after a PAUSE command.
	mov ah,9		; string output
	int 0x21		; DOS API
		; to be safe, we issue an extra pause command:
	mov byte [rcmd],0x85	; PAUSE audio
	mov byte [rlen],0x0d	; request header length
	call CDRDEV
	; jc "warning: pause command failed"
cpi3i:	pop ax

cpi4:	test ah,8		; no disk in drive?
	jz cpi5
	mov dx,nodiskmsg	; complain about missing disk
	jmp failed

cpi5:  	; test al,2 - door unlocked if NZ
	; test al,4 - can read audio to file if NZ
	; test al,0x20 - can do interleave if NZ
	; test al,0x40 - can do prefetch if NZ
	; test ah,2 - all functions accept RedBook addresses

	; could use IOCTLR 8 to get a dword "data disk size" now
	; could use IOCTLR 9, check disk change: 1 Y, -1 N, 0 ?.

	jmp short cpitry	; yes, all preconditions met!

cpixyz:	mov dx,notracksmsg	; complain: no track numbers!
	jmp failed

cpitry:	; drive / disk are THERE and READY and ABLE to play CDDA
	mov byte [dsize],1+6	; returns 2 bytes and a dword...
	mov byte [thedata],0x0a	; data type: audio track count
	call IOCTLR		; IOCTL READ
	jc cpixyz		; on error, give up

	mov ax,[ds:thedata+3]	; copy end time of last track LO
	mov [totrk],ax
	mov ax,[ds:thedata+5]	; copy end time of last track HI
	mov ah,0
	mov [totrk+2],ax
	mov ax,[ds:thedata+1]	; get min and max track number
	mov [mintrk],al
	mov [maxtrk],ah
	aam			; convert AL to BCD
	add ax,"00"		; convert to ASCII
	xchg al,ah		; put more signif. digit in AL
				; (first in string) etc.
	mov [ytrack1],ax
	;
	mov al,[ds:maxtrk]
	aam			; convert AL to BCD
	add ax,"00"		; convert to ASCII
	xchg al,ah		; put more signif. digit in AL
				; (first in string) etc.
	mov [ytrack2],ax

	mov dx,ytrackmsg	; show valid track number range
	mov ah,9		; string output
	int 0x21

	mov al,[ds:playbin]	; which track to play
	cmp al,-1		; none? Fine then.
	jnz cdpl1		; else play that ONE track
	jmp showtoconly		; show the audio TOC instead

invtrk:	mov dx,nosuchtrack	; complain: invalid track number
	jmp failed

cdpl1:	cmp al,[ds:mintrk]	; below minimum allowed number?
	jb invtrk
	cmp al,[ds:maxtrk]	; above maximum allowed number?
	ja invtrk
	;
	mov ah,al		; pass track number to driver
	mov al,0x0b		; data type: track starting time
	mov [thedata],ax	; set up IOCTL read query packet
	mov byte [dsize],2+6	; returns a dd (and a flags dw)
	call IOCTLR		
	jnc cdpl2
	mov dx,notocstart	; complain: no starting time
	jmp failed

cdpl2:	mov ax,[thedata+2]	; fetch track starting point LO
	mov [fromtrk],ax
	mov ax,[thedata+4]	; fetch track starting point HI
	mov ah,0
	mov [fromtrk+2],ax
	mov al,[ds:playbin]	; which track?
	cmp al,[ds:maxtrk]	; if the last one, end already
	jz cdpl5	; ***	;   known. Else, end is the
	inc ax			; beginning of the next track!
cdpl3:
	mov ah,al		; pass track number to driver
	mov al,0x0b		; data type: track starting time
	mov [thedata],ax	; set up IOCTL read query packet
	mov byte [dsize],2+6	; returns a dd (and a flags dw)
	call IOCTLR		
	jnc cdpl4
	mov dx,notocend		; complain: no end time, so we
				; assume track end at disk end
	mov ah,9		; string output
	int 0x21		; DOS API

cdpl4:	mov ax,[thedata+2]	; fetch NEXT track start LO
	mov [totrk],ax		; ... as END point of THIS track
	mov ax,[thedata+4]	; fetch NEXT track start HI
	mov ah,0
	mov [totrk+2],ax	; ... as END point of THIS track

cdpl5:	; *** now we know totrk and fromtrk RedBook track times
	; *** we must use those to calculate track SIZE, too.

	; HSG address = (((60*m)+s)*75)+150, RB address = f,s,m,0
	mov bx,totrk		; RedBook end of track pointer
	mov di,tostring		; string place for track end
	call showRB
	call rb2hsg		; convert [bx] RB to dx:ax HSG
	mov [rheader+0x12],ax	; store potential size of track
	mov [rheader+0x14],dx	; store potential size of track
	mov bx,fromtrk		; RedBook start of track pointer
	mov di,fromstring	; string place for track start
	call showRB
	call rb2hsg		; convert [bx] RB to dx:ax HSG
	mov [rheader+0x0e],ax	; store *HSG* start position!
	mov [rheader+0x10],dx	; store *HSG* start position!
	sub [rheader+0x12],ax	; subtract size of prev. tracks
	sbb [rheader+0x14],dx	; subtract size of prev. tracks
	test word [rheader+0x14],0xfff8
	jz cdpl6		; duration <= 0x7ffff ?
	mov dx,negdurationmsg	; block > 116 minutes duration!
	mov ah,9		; string output
	int 0x21		; DOS API
	;
	mov ax,60 * 75		; just try 1 minute as default
	mov [rheader+0x12],ax	; (if duration was unbelievable)
	xor ax,ax
	mov [rheader+0x14],ax

cdpl6:
	; now DURATION is stored as a dword, unit is frames,
	; each frame is 1/75 seconds long. Could DISPLAY it.

	mov dx,durationmsg	; show mm:ss - mm:ss playing
				; time (not duration itself)
	mov ah,9		; string output
	int 0x21		; DOS API

	mov byte [rheader+0x0d],0	; using HSG start pos.
	mov byte [rlen],0x16		; request header size
	mov byte [rcmd],0x84		; command: PLAY AUDIO
	call CDRDEV
	jc playCY
	mov dx,playackmsg	; confirm playing
	mov ah,9		; string output
	int 0x21		; DOS API
	clc				; return as OKAY
	ret				; FINALLY, we are done.

playCY:	mov dx,cannotplaymsg	; complain: audio play error
	jmp failed		; return with ERROR

showtoconly:
	mov dx,notoclistmsg	; excuse the lack of features...
	mov ah,9		; string output
	int 0x21		; DOS API
	;
	mov word [thedata],0x010c	; data type 0x0C: Q-chn.
	mov byte [dsize],2+2+4+4	; space for Q channel!
	call IOCTLR
	jc noaudiocursor	; Q channel read worked?
	;
	mov ax,[ds:thedata+2]	; AL track number (BCD!)
	or ax,ax		; any nonzero value?
	jz noaudiocursor	; else skip position display!
	push ax			; AH subtrack index (BCD!)
	mov cl,4
	shr al,cl
	add al,"0"
	mov [acursT],al		; high digit track
	pop ax
	and al,15
	add al,"0"
	mov [acursT+1],al	; low digit track
	mov al,ah		; fetch index
	shr al,cl
	add al,"0"
	mov [acursT+3],al	; high digit index
	mov al,ah
	and al,15
	add al,"0"
	mov [acursT+4],al	; low digit index
	;
	mov al,[thedata+4]	; Q-channel uses M, S, F !
	mov ah,[thedata+6]
	mov [thedata+4],ah	; translate into RedBook F, S, M.
	mov [thedata+6],al
	mov al,[thedata+8]	; Same Q-channel translation...
	mov ah,[thedata+10]
	mov [thedata+8],ah	; M,S,F,0 -> F,S,M,0 again.
	mov [thedata+10],al
	;
	mov bx,thedata+4	; relative RedBook playing pos.
	mov di,acursR
	call showRB
	mov bx,thedata+8	; absolute RedBook playing pos.
	mov di,acursA
	call showRB
	mov dx,cddacursormsg	; show audio status / Q channel
	mov ah,9		; string output
	int 0x21		; DOS API

noaudiocursor:
	clc			; success
	ret

%endif

; -----------------------------

%ifdef CDDA

rb2hsg:	push cx			; convert [ds:bx] RedBook to
	xor cx,cx		; HSG address in dx:ax
	mov al,[ds:bx+2]	; minutes
	mov ah,60		; seconds per minute
	mul ah
	add al,[ds:bx+1]	; seconds
	adc ah,cl
	mov dx,75		; frames per second
	mul dx
	add al,[ds:bx]		; frames
	adc ah,cl
	adc dx,cx
	mov cx,150		; HSG offset MINUS 2 seconds
	sub ax,cx
	sbb dx,0
	pop cx
	ret

showRB:	push di			; convert [ds:bx] RedBook to
	push ax			; mm:ss string at [ds:di] ...
	mov al,[ds:bx+2]	; minutes
	aam			; convert AL to BCD
	add ax,"00"		; convert to ASCII
	mov [di],ah		; 10s digit first
	inc di
	mov [di],al
	inc di
	mov byte [di],":"
	inc di
	mov al,[ds:bx+1]	; seconds
	aam			; convert AL to BCD
	add ax,"00"		; convert to ASCII
	mov [di],ah		; 10s digit first
	inc di
	mov [di],al
	inc di
	mov byte [di],"."
	inc di
	mov al,[ds:bx+0]	; frames
	aam			; convert AL to BCD
	add ax,"00"		; convert to ASCII
	mov [di],ah		; 10s digit first
	inc di
	mov [di],al
	pop ax
	pop di
	ret

%endif

; -----------------------------

%ifdef CDDA
IOCTLR:	mov byte [rcmd],3	; command: IOCTL read
	jmp short ioctlX
%endif

	; do CD-ROM IOCTL command AL and return to caller

%ifndef CDDA
IOCTLW:
%endif
IOCTLW2:	
	mov [thedata],ax	; command AL, argument AH
%ifdef CDDA
	mov byte [rcmd],0x0c	; command: IOCTL write
ioctlX:	mov byte [rheader],20	; 20 bytes IOCTL request header
	mov word [datP],thedata	; pointer to data - offset
	mov word [datS],ds	; pointer to data - segment
%endif

CDRDEV:	mov ax,0x1510	; CD-ROM device call through *CDEX
	xor cx,cx
%ifdef CDDA
	mov [rstatus],cx	; zap status
%endif
	mov cl,[ds:seldrv]	; selected drive (0 = A:...)
	mov bx,rheader	; request header (in segment ES)
	int 0x2f	; MUX
	jc cdxerr	; any *CDEX error?
	mov ax,[ds:rstatus]	; status of the request
	test ax,0x8000	; error? (... 0x0200 busy 0x0100 okay)
	jz cdxok	; else we are done
%ifdef CDDA
	push ax
	push bx
	mov bx,ax
	and bx,15	; low nibble of error code (AL)
	mov ah,[hexdigits+bx]	; translate to ASCII
	mov [cdderrNUM+1],ah
	shr ax,1
	shr ax,1
	shr ax,1
	shr ax,1	; now high nibble is shifted into low
	mov bx,ax	; nibble...
	and bx,15	; high nibble of error code
	mov al,[hexdigits+bx]	; translate to ASCII
	mov [cdderrNUM],al
	pop bx
	pop ax
%else
	nop		; *** error code is in AL, insert int3
			; *** here if you want to debug things
%endif
	mov dx,cdderr	; CD-ROM sys driver returned an error
	jmp short cdxe2

cdxok:	clc		; return without error
	ret

cdxerr: mov dx,cdxerri	; error message: no IOCTL possible
	cmp al,1	; invalid function?
	jz cdxe2
	mov dx,cdxerrd	; error message: no such CD-ROM drive
	cmp al,0x0f	; invalid drive?
	jz cdxe2
	mov dx,cdxerrx	; error message: unknown error
cdxe2:	mov ah,9	; string output
	int 0x21	; DOS API
	stc		; return with error
	ret

; -----------------------------

rheader:
rlen	db 20	; 00 length
	db 0	; 01 subunit, filled in by *CDEX
rcmd	db 0x0c	; 02 command: IOCTL write
rstatus	dw 0	; 03 status, filled in by CD-ROM sys driver
	dd 0,0	; 05 (reserved)
		; rest differs for other calls than IOCTL!
	db 0xf8	; 0d if IOCTL: media descriptor
datP	dw thedata	; pointer to IOCTL data, offset
datS	dw 0	; pointer to IOCTL data, segment
dsize	dw 1	; size of IOCTL data (normally 1 for us)

		; data for command PLAY AUDIO is longer than that!
		; we simply overflow it into the "thedata" area.

	; RBIL calls the next structure "C?-ROM control block"
thedata	db 0	; the command (write) / requested data type (read)
	db 0	; the argument (lock/unlock only)
%ifdef CDDA
trkOffs	dd 0	; track starting point (IOCTL read 0x0b)
	dw 0	; track flags (IOCTL read 0x0b)
		; (OTHER ioctl reads use less data)
	dw 0	; ... but read Q subchannel uses more!
%endif

; -----------------------------

seldrv	db -1		; drive selected by the user

mindrv	db 0		; first CD-ROM drive number
ndrives	db 0		; number of CD-ROM drives

; -----------------------------

%ifdef CDDA

playbin	db 0	; binary form of selected track number
mintrk	db 0	; minimum track number
maxtrk	db 0	; maximum track number
fromtrk	dd 0	; STARTING time (RedBook format)
totrk	dd 0	; END time (RedBook format)

playmsg	db 13,10,"Playing audio track "
playasc	db "00",13,10,"$"

cdwrmsg	db "CD/DVD writer detected.",13,10,"$"
doormsg	db "Please close drive door and try again.",13,10,"$"
nocddamsg:
	db "Sorry, this drive cannot play CDDA directly.",13,10,"$"
nodiskmsg:
	db "No disk in drive.",13,10,"$"
cdbusymsg:
	db "Already playing? Sending PAUSE command to be safe..."
	db 13,10,"$"

notracksmsg:
	db "Audio session empty or no valid audio tracks.",13,10,"$"
playsyntaxmsg:
	db "Syntax error. Track number must follow PLAY",13,10
	db "command right away. Example: CDROM PLAY12",13,10,"$"

ytrackmsg:
	db "Valid audio track numbers: "
ytrack1	db "00-"
ytrack2	db "00",13,10,"$"

cddacursormsg:
	db "Audio play in progress. Q-Channel tells:",13,10
	db "Track "
acursT	db "xx.xx, relative position "
acursR	db "xx:xx.xx, absolute position "
acursA	db "xx:xx.xx",13,10,"$"

durationmsg:
	db "Playing audio from seek position "
fromstring:
	db "xx:xx.xx to "
tostring:
	db "xx:xx.xx",13,10,"$"

negdurationmsg:
	db "Negative or huge duration found - changing to "
	db " 1 minute.",13,10,"$"
nosuchtrack:
	db "You must specify a valid audio track number.",13,10
	db "(See the above message about the allowed range!)"
	db 13,10,"$"
notocstart:
	db "Cannot tell where this track starts - giving up."
	db 13,10,"$"
notocend:
	db "Track seems endless - guessing length.",13,10,"$"

notoclistmsg:
	db "To see individual track start and end times,",13,10
	db "please PLAY the track in question. No track",13,10
	db "listing to screen implemented yet...",13,10,"$"

playackmsg:
	db "Audio playing should now start. Returning to DOS."
	db 13,10,"$"
cannotplaymsg:
	db "Could not start to play audio track - giving up."
	db 13,10,"$"

%endif

; -----------------------------

helpmsg	db 13,10
%ifdef CDDA
	db "Freeware CDROM toolkit with audio support - written and"
	db 13,10
	db "conceived in 2003 by Eric Auer <eric*coli.uni-sb.de>"
	db 13,10,13,10
%endif
	db "Usage: CDROM [EJECT|CLOSE|LOCK|UNLOCK|RESET] X:",13,10
	db "Example: CDROM EJECT N:",13,10
%ifdef CDDA
	db 13,10,"Audio: CDROM [PAUSE|CONT|INFO|PLAYnn] X:"
	db 13,10,"(xx is audio track number)",13,10
%endif
	db 13,10,"Ask *CDEX 2.0+ if a CD-ROM exists: CDROM N:",13,10
	db 13,10,"Errorlevels: 0 okay, 1 failed / no such drive."
%ifdef CDDA
	db 13,10,"Shows a list of valid drives if invalid selection."
%endif
	db 13,10,"Not all drives do all commands.",13,10
	db "$"


cdxerrx	db "Unknown *CDEX error.",13,10,"$"
cdxerrd	db "CD-ROM drive not known to *CDEX.",13,10,"$"

%ifdef CDDA
hexdigits:
	db "0123456789abcdef"
cdderr	db "CD-ROM sys driver reports failure. Error code: "
cdderrNUM:
	db "xx.",13,10,"$"
cdxerri	db "Required command not supported by drive or driver.\n"
	db 13,10,"$"
%else
cdderr	db "CD-ROM sys driver reports failure.",13,10,"$"
cdxerri db "Drive IOCTL not supported!?",13,10,"$"
%endif

; -----------------------------

	; command table: pointers to strings and functions
	; "unlock" is before "lock", as both contain "lock"!
cmds	dw ejectW,  ejectC,  closeW,  closeC,  ulockW,  ulockC
	dw lockW,   lockC,   resetW,  resetC
%ifdef CDDA
	; "info" is before "continue" as both contain "in"!
	dw pauseW,  pauseC,  infoW,   infoC,   contW,   contC
	dw playW,   playC
%endif
	db 0,      0

	; command names: only first 2 chars are checked now ;-)
ejectW	db "EJECT$"	; EJ
closeW	db "CLOSE$"	; CL
lockW	db "LOCK$"	; LO <-
ulockW	db "UNLOCK$"	; UN
resetW	db "RESET$"	; RE
%ifdef CDDA
pauseW	db "PAUSE$"	; PA
infoW	db "INFO$"	; IN <-
contW	db "CONT$"	; CO <- (avoid the "IN" in "CONTINUE")
playW	db "PLAY$"	; PL
%endif

; -----------------------------

donemsg	db "Done.",13,10,"$"
failmsg	db "Failed.",13,10,"$"
cmdmsg	db "Command: $"

cdx1msg	db "Using old *CDEX 1.x: CD-ROM range is "
cdx1beg	db "A: - "
cdx1end	db "A: !?"	; continued in crlfmsg to save space!
crlfmsg	db 13,10,"$"

nocddrv db "A: is no CD-ROM drive, but < $"
colon	db ": $"
nocdONE	db "> is.$",13,10,"$"
nocdN	db "> are.$",13,10,"$"

nocdrom	db "No *CDEX (SHSUCDX...) found, no CD-ROM access."
	db 13,10,"$"

drives	db -1		; array with drive numbers
	;         times 32 db -1	; now in BSS, done at runtime

