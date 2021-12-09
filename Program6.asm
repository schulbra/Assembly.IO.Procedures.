TITLE Project #6     (Program#6.asm)
;________|Header Block|______________________________________________________________________________________________________|
;-       -Author:                          -Brandon Schultz                                                                  |
;--       -Date:                           -11.27.21                                                                         |
;---       -OSU email address:             -schulbra@oregonstate.edu                                                         |
;----       -Course Section:               -CS 271 400 F2021                                                                 |
;------     -Assignment:                   -Program #6                                                                       |
;-------       -Description:                                                                                                 |
;-------        - This program prompts a user to enter as input ten signed integers. It then validates the users input       |
;-------          for compatibility in regards to the int's individual size and ability to fit within 32 a bit register,     | 
;-------          before displaying all valid input as a list of ten items, the lists Sum and it's rounded average. Of note, | 
;-------          this program uses Irvine's ReadString and WriteString procedures to get input from and display output to   |
;-------          it's users.                                                                                              //
;--------       -References:                                                                                               |
;--------         -    https://faculty.tamuc.edu/nsirakov/Teaching/lecturepowerpoints/Lecture_28.pdf                       |
;--------         -    http://www.cs.cmu.edu/afs/cs/academic/class/15213-f02/www/R03/section_b/x86notes.pdf                |
;--------         -    https://www.cs.virginia.edu/~evans/cs216/guides/x86.html                                            |
;--------         -    https://faydoc.tripod.com/cpu/stosb.htm                                                             |
;--------         -    http://digitallocha.blogspot.com/2014/01/what-is-1013-in-assembly-language.html                     |
;_________________________________________________________________________________________________________________________//
INCLUDE Irvine32.inc
;__________
; Macros:  |
;__________|

;_|mGetString|______________________________________________________________________________________________
;  - This macro is used in the obtaining and reading of a input string from the user.                       |
;   - WriteString is implemented as a means to prompt, and then obtain an input string from the user.       |
;   - ReadString is then used to read and store this same input string.   							        |
;   -- Input/Pass By Reference Parameters:                                                                  |
;      - addressPrompt:         Adddress of prompt asking for Users Input to be passed onto stack.          |
;      - addressUI:             Address of arr used for storage of users input values.                      |
;      - addressStrByRead:	    Adrress of DWORD var used to keep count of the read-in string's byte total. |
;   -- Input/Pass By Value Parameters:                                                                      |
;      - MaxLengthPerUIValue:   Input strings max length value                                              |
;		-- Output/Pass By Reference Parameters parameters:							                        |
;      - addressUI:             Contains memory address for starting point of string read.                  |
;      - addressUI:             Contains memory address for count of bytes so far read.                     |
;__________________________________________________________________________________________________________//	
mGetString macro addressPrompt, addressUI, addressStrByRead
	  push	  EDX                         ; Store register values.
    push    ECX
    push    EAX
    push    EDI
    mov     EDX,addressPrompt           ; Prompt user for input val.   			 
    call    WriteString                 ; Users input is read & stored using ReadString proc:                   
    mov     EDX, addressUI             
    mov     ECX, MaxLengthPerUIValue    ; Used to manage length of input string handled
    call    ReadString
    mov     EDI, addressStrByRead	    	; Used in keeping track of bytes read in by macro
    mov     [EDI], EAX						
    pop     EDI                         ; Restore register values.
    pop     EAX
    pop     ECX
    pop     EDX
endm
; ___|mDisplayString|________________________________________________________________________
;  - Macro that when given an address of a byte string, will print said byte string contents |
;  using the WriteString proc.                                                               |
; -- Input/Pass By Reference Parameters:                                                     |
;   - addressStringPrinted: Memory address of string thsat will be displayed                 |
; -- Output parameters: N/A.                                                                 |
;___________________________________________________________________________________________//         
mDisplayString macro addressStringPrinted
    push    EDX
    mov     EDX,addressStringPrinted
    call    WriteString
    pop     EDX
endm
;_____________________________________
;Constants used throughout program 6: |
;_____________________________________|
MaxLengthPerUIValue = 30    ; Max number of digits a user can enter.
MaxNumUIValues = 10         ; The ten input items to obtain from user.
;SEPARATOR equ ' '
;____________________________________________________________________________
; OFFSETs utilized by procedures to access stack parameters                   |
;  -Additional informaiton is provided in comments for individual PROCs below.|
;_____________________________________________________________________________
;para_1 EQU [EBP + 8]
;para_2 EQU [EBP + 12]
;para_3 EQU [EBP + 16]
;para_4 EQU [EBP + 20]
;para_5 EQU [EBP + 24]
;para_6 EQU [EBP + 28]

.data
displayIntroductionPromptA       BYTE	"     ____________________________________________________________________  ",13,10,0
displayIntroductionPromptB       BYTE   "    / -    Author: Brandon Schultz                                      /  ",13,10,0 
displayIntroductionPromptC		 BYTE	"   /  --   Assignment: Project 6 - String Primitives and Macros        /   ",13,10,0
displayIntroductionPromptD		 BYTE	"  /   ---  **EC1: UI values are numbered                              /    ",13,10,0 
displayIntroductionPromptDEC     BYTE	" /    ---- **EC2: Displays rounded avg float of UI's Ten values.     /    ",13,10,0 
displayIntroductionPromptE       BYTE   "/___________________________________________________________________/       ",13,10,13,10,0  
divider                          BYTE   "_____________________________________________________________________	    ",13,10,0                                      
displayInfoInstructionsPromptC   BYTE   "\       -  Please provide this program with 10 signed decimals.      \     ",13,10,0 
displayInfoInstructionsPromptD   BYTE   " \     --  Each integer must be able to fit within a 32 bit register. \    ",13,10,0   
displayInfoInstructionsPromptE   BYTE   "  \   ---  On valid input, this program will display the ten integers  \   ",13,10,0
displayInfoInstructionsPromptF   BYTE   "   \ ---- and their cumulative Sum and Mean.                            \  ",13,10,0
displayInfoInstructionsPromptG   BYTE   "    \____________________________________________________________________\ ",13,10,13,10,0 
UIInputLineNum	                 DWORD  ?
dividerD                         BYTE   "____                                                                    	    ",13,10,0 
dividerDTop                      BYTE   "    \                                                                         ",13,10,0
displayUserInputPromptA          BYTE   "     \_____________________________________________________________________     ",13,10,0
displayUserInputPromptB          BYTE   "Input \  -  These are the 10 valid numbers that you entered:               \ 	",13,10,0
displayUserInputPromptC          BYTE   "Values:\ -- Various caulculations with your input is shown further below:   \ 	",13,10,0
displayUserInputPrompt           BYTE   " _______\____________________________________________________________________\  ",0;13,10,0
displaysUIStringAsLine           BYTE   ", " ,0	 
displaysUIStringAsLineSpacer     BYTE   "  ",0
dividerETop                      BYTE   "                                                                             ",13,10,0
dividerE                         BYTE   "         ___________________________________________                                ",13,10,0
displayUISumPrompt               BYTE   "        / -  Their Sum Is:            ",0     
displayUIMeanPrompt              BYTE   "       / --  Their Mean is:           ",0     
inputTotal                       BYTE   "      / ---  Valid Input Count:       ",0
roundedAvg                       BYTE   "     / ----  Rounded Avg Float:       ",0
dividerEBot                      BYTE   "    /_____________________________________________                                  ",13,10,0
dividerF                         BYTE   "     \  ----- Goodbye!                     \                                ",13,10,0
dividerG                         BYTE   "      \_____________________________________\                               ",13,10,0
;displayFarewellPrompt           BYTE   " I LOVE YOU! ",0
dividerB                         BYTE   "______________________________________________________________________________",13,10,0
dividerC                         BYTE   "Line     / -  Below you will be prompted for input of ten signed integers:   / 	",13,10,0
dividerCc                        BYTE   "Number: /  -- If you enter other things, I will repeat myself endlessly.    / 	",13,10,0
dividerCcc                       BYTE   "_______/___________________________________________________________________/ 	",13,10,0
UILineForm		                   BYTE   ".     |",0
getInputPrompt                   BYTE   "- Please Enter an Signed Number: ",0
;dividerD                        BYTE   "___________________________________________/	",13,10,0
validationFail                   BYTE   "       | ERROR: ",0
UIString                         BYTE   MaxLengthPerUIValue dup(?)
stringInputVal                   BYTE   MaxLengthPerUIValue dup(?)
inputVals                        SDWORD MaxNumUIValues dup(?)
UIStringLengthCount              DWORD  ?
inputVal                         DWORD  ?
Sum                              SDWORD ?
Mean                             SDWORD ?
roundInt					     DWORD	0
roundFactor					     REAL8	-0.0005
roundFloat					     REAL8	?
thousand					     WORD	1000
floatInstruct					 WORD	110000000000b


.code
main PROC
;_______________________________________________________________________________
; - Introduces and displays text regarding programmer,information on programs   |
;  functionality and instructions using mDisplayString macro.                   | 
; - Programs Introductory prompts and directions/boarders                       |
;_______________________________________________________________________________|
mDisplayString  OFFSET displayIntroductionPromptA
mDisplayString  OFFSET displayIntroductionPromptB
mDisplayString  OFFSET displayIntroductionPromptC
mDisplayString  OFFSET displayIntroductionPromptD
mDisplayString  OFFSET displayIntroductionPromptDEC
mDisplayString  OFFSET displayIntroductionPromptE
mDisplayString  OFFSET divider
mDisplayString  OFFSET displayInfoInstructionsPromptC
mDisplayString  OFFSET displayInfoInstructionsPromptD
mDisplayString  OFFSET displayInfoInstructionsPromptE
mDisplayString  OFFSET displayInfoInstructionsPromptF
mDisplayString  OFFSET displayInfoInstructionsPromptG
mDisplayString  OFFSET dividerB
mDisplayString  OFFSET dividerC
mDisplayString  OFFSET dividerCc
mDisplayString  OFFSET dividerCcc
;call    intro
 ;_____________________________________________________________________
 ;-Obtains input values, placing said elements in arr of length = 10,  |
 ;  representative of the 10 values to be obtained from user.         / 
 ;___________________________________________________________________/
 mov  ECX,MaxNumUIValues
 mov  EDI,OFFSET inputVals
 ;_____________________________________________________________________________________
 ; Test programs used to get 10 valid integers from the user.  ReadVal is called within
 ; main loop to invoke the mGetString macro, which will get user input in the form of a 
 ; string of digits and then validate and potentially convert the string of ascii digits to 
 ; its numeric value representation. Additional context is provided below:
 ;_____________________________________________________________________________________
 getUINums:
    ; - Prompt for stating to user that input doesnt fit within 32 bit reg                                                            
    push   OFFSET validationFail                                                   
    push   OFFSET getInputPrompt ; - Prompt for obtaining UI data.                                                     
    push   OFFSET UIString       ; - Where string of UI is stored.
    ; - Where length of string version of UI data is kept as a DWORD var
    push   OFFSET UIStringLengthCount        
    push   OFFSET inputVal       ; - Where validated signed val is stored
    ; mov   dh, SEPARATOR
    call    ReadVal              ; 
    mov    EBX,inputVal          ; - Places inputVal into curr index of inputVals. 
    mov    [EDI],EBX             ;    Then prepares next index in inputVals array 
    add    EDI, type inputVals   ;    for the next input val to be read in until
    loop   getUINums             ;    ten integers are entered.
    mDisplayString  OFFSET dividerD
    mDisplayString  OFFSET dividerDTop
    mDisplayString  OFFSET displayUserInputPromptA
    mDisplayString  OFFSET displayUserInputPromptB  
    mDisplayString  OFFSET displayUserInputPromptC
    mDisplayString  OFFSET displayUserInputPrompt
    call   CrLf
    mov    ESI,OFFSET inputVals
    mov    ECX,MaxNumUIValues
    ;mDisplayString  OFFSET dividerE
 ;_______________________________________________________________________________________
 ; -  Loop used  to find Sum of UI values before calling WriteVal and increasing counter.\__
 ;  - Sum value will be stored in EBX and is used again  to calculate avg value that is then\ 
 ;   displayed to user                                                                      |
 ;__________________________________________________________________________________________|
 wantSum:
    mov    EBX,[ESI]             ; - Adds current input val to current Sum until        
    add    Sum,EBX               ;  MaxNumUIValues has no remaining indcies to fill 
    add    ESI,type inputVals    ;  or count hits zero.                                  
    push    EBX                  ; - Displays inputVals Sum before going to WriteVal:          
    push    OFFSET stringInputVal    
    push    lengthof stringInputVal
    call    WriteVal
    cmp     ECX,1
    je      timeToGetSum 
    mDisplayString  OFFSET displaysUIStringAsLine 
    mDisplayString  OFFSET displaysUIStringAsLineSpacer
    loop    wantSum
    timeToGetSum:
    call    CrLf
 ;___________________________________________________________________________________
 ; - Finds/displays Sum, then uses Sum value to find/display average of 10 UI items: \
 ;________                                                                           | 
 getSum:; \_________________________________________________________________________/                                                                  
    mov     EAX,Sum                         ;  - Sum of all ten UI values                            
    mov     EBX,MaxNumUIValues              ;  - 10                     
    cdq                                     ;  - Sign-extends EAX into EDX:EAX        
    idiv    EBX                             ;  to allow for division                  
    mov     Mean,EAX                        ;  - (Sum of UI)/10 = ya boi EAX       
    mDisplayString  OFFSET dividerETop 
    mDisplayString  OFFSET dividerE                                                                  
    mDisplayString  OFFSET displayUISumPrompt ; -prints Sum
    push    Sum
    push    OFFSET stringInputVal           ; -Sum value string is used in avg       
    push    lengthof stringInputVal         ; caulculation below. Sum val stored as  
    call    WriteVal                        ; string via stringInputVal.             
    call    CrLf                                                               
    mDisplayString  OFFSET displayUIMeanPrompt  ; -prints mean/avg. 
    push    Mean
    push    OFFSET stringInputVal           ; - avg val is stored as string using    
    push    lengthof stringInputVal         ; stringInputVal
    call    WriteVal
    call    CrLf

   ;______________________________________________
  ; - Methods for numbering lines of  UI values: |
  ;_____________________________________________/ 
	mDisplayString OFFSET inputTotal
	mov EAX, UIInputLineNum
	call WriteDec
	call CrLf

  ; - ran out of time to properly implement
  roundedIntToFloat:

		fnstcw	floatInstruct				; Transfers floating point status to int value.   
		mov	bx, floatInstruct             
		and	bh, 11110011b					; keeps instruction after removing bits.
		or	bh, 00001100b					; remaining bits round towards zero
		mov	floatInstruct, bx			
		fldcw	floatInstruct				; loads instruction held by control word
		fild	Sum
		fidiv	inputVals
		fadd	roundFactor
		fimul	thousand
		frndint
		fidiv	thousand					; num is returned as decimal after rounded to .001
		fstp	roundFloat					; returned num is saved

        ;mDisplayString OFFSET  roundedAvg
     
		mov	edx, OFFSET roundedAvg
		call	WriteString
		fld	roundFloat
		call	WriteFloat
		call	Crlf

  mDisplayString  OFFSET dividerEBot
  mDisplayString  OFFSET dividerF 
  mDisplayString  OFFSET dividerG
  ; - Goodbye prompt to display at programs end:
  ;mDisplayString  OFFSET displayFarewellPrompt
  Invoke ExitProcess, 0      
	call CrLf
main ENDP

;___|ReadVal|__________________________________________________________________________________
; - Proc used to invoke the mgetString macro                                                   |
; -- Obtains the user’s input string of digits, validates input's ability to fit within a      |
;    32 bit register and converts this input to its numeric equivalent using string primitives |                 
; --- Input/Pass By Address Parameters:                                                        |
;   -- Memory addresses of:                                                                    |
;       - validationFail:    Address for inputValidation failure prompt                        |
;       -- getInputPrompt:   Address of Prompt for obtaining UI data.                          |
;       --- UIString:        Address containing string version of users input data             |
;       ---- UIStringLengthCount:        Where length of string variant of UI data is          |
;             stored  as a DWORD var.                                                          |
;       ----- inputVal:      Where validated signed val is stored as a SDWORD var.             |
; --- Output parameters:                                                                       |
;       -    Byte count of UI string, or value held at mem address of UIStringLengthCount.     \       
;       --   Converted numeric SDWORD value as a string of ascii digits, or val held at address |
;       --   of UIString.                          ____________________________________________/
;       ---  inputVals stored, signed int values. /
;________________________________________________/
ReadVal PROC
; - inputValPorN: Checks UI value for being negative or positive:
;   P = non signed int =  0    ||   N = neg, signed int = 1
; -tenTimesMultiply: = 10 . Will be used to multiply by 10 to
; account for all  vals for requirtements like caulculating the UI mean.       
 LOCAL inputValPorN:DWORD, tenTimesMultiply:DWORD
 pushad
  ; Methods for numbering UI one value at a time:                                            
 	mov EAX,1
	add UIInputLineNum,EAX		
	mov EAX, UIInputLineNum     ; numbered line 1-10
	call WriteDec
	mov EDX, OFFSET UILineForm  ; |
	call WriteString
 ; Prompt for user input:   
 getUIPrompt:
 ; Methods for getting and validating user input:
 mGetString [EBP+20],[EBP+16],[EBP+12]            
 ; [EBP+20] -getInputPrompt taken as input 
 ; [EBP+16]  -UIString output via lodsb
 ; [EBP+12]  -bytecount val stored as output via EBP           
    mov inputValPorN,0      ; -   P = non signed int =  0    ||   N = neg, signed int = 1
    mov tenTimesMultiply,10 ; - multiply curr  digit of string by ten  
    mov     EBX,0           ; - add value to EBX, or UI's curr Sum value
    mov     ESI,[EBP+16]
    mov     EAX,[EBP+12]     
    mov     ECX,[EAX]       ; - Continues through each digit of UI string.
    cmp     ECX,12
    ja      UIValidationFail ; Non 0-9 input detected
    mov     EAX,0
    cld
  ;__________________________________________________________________________________________________
  ; Conversion routine for the string of ascii digits to its numeric value representation:           |
  ;  - Val at digit 1 is loaded in AL reg using lodsb operator.                                      |
  ;  - Val in EAX is pushed for later use, and UIStringLengthCount is moved into EAX.                |
  ;  - Digit 1 of string could not be a numerical value, but instead either - or +.                  |
  ;  -  This is determined  via the comparison between ECX, [EAX]. Returned val (+ or -)             |
  ;   is saved via popping EAX below                                                                 |         
  ;___________________________________________________________________________________________________
  validateUIStringDigits:                                                                           
    lodsb                                                                                           
    push    EAX                                                                                    
    mov     EAX,[EBP+12]                                                                         
    cmp     ECX,[EAX]                                                                            
    pop     EAX                     ;_________________________________________________________                                                        
    je      UIStrDigitOne           ;   - If first digit of string, jump to sign determination|        
    jmp     UIStrDigitsAfterOne     ;   - If not 1st digit, skip routine and validate if      |  
                                    ;   - input is a valid 0-9 number.                        |                 
                                    ;________________________________________________________/ 
  ;_______________________________________________________________________________________                          
  ; -Determines if strings first digit is + or - by comparing val loaded in AL against 45 \
  ;  ( ASCII 45= negative sign char) and 43 ( ASCII 43= positive sign char)               |
  ;_______________________________________________________________________________________|  
  UIStrDigitOne:
    cmp     AL,45
    je      UIStrDigitOne_IsNeg
    cmp     AL,43
    je      UIStrDigitOne_IsPos
    jmp     UIStrDigitsAfterOne
   ;______________________________________________________________________ 
   ; - First digit of UI string is determined to be signed                | 
   ; set cld flag to 1:                                                   |
   ;________________ _____________________________________________________|
   UIStrDigitOne_IsNeg:
    mov     inputValPorN,1
    loop    validateUIStringDigits     ; - First digit of UI is positive,
   UIStrDigitOne_IsPos:
    ; Keep loop going as we now need the next inputVal from UI string     
    loop    validateUIStringDigits
 ;___________________________________________________________________________
 ; For all digits after MSB, input vals 0-9 are processed by using 48 and 57, \
 ; ( ASCII 48 = code point for zero char ) and                                 |
 ; ( ASCII 57= code point for Nine char)                                       |
 ;_____________________________________________________________________________
 UIStrDigitsAfterOne:;                                                           
    ; Values is less than 0, sign bit has already been determined at this point  
    ; in readVal proc so user is up to no good. 
    cmp     AL, 48
    jl      UIValidationFail  ; Value is greater than 9.  
    cmp     AL, 57
    jg      UIValidationFail 

  ;___________________________________________________________________________________
  ; -On successful input validation ascii will be converted to numeric:               \
  ; - al reg will hold some value between 48 to 57 if ReadVal runs to point of UI       \
  ;  validation having passed so this can only be done when al = n where 48<n<57.        \
  ; - after conversion, EAX reg is used to multiply Sum value at point of particular      |
  ; post-digit conversion by ten via tenTimesMultiply as a means to eventually finding   /
  ; avg value and to store said digit before moving to next digit of string to process. /
  ;___________________________________________________________________________________ /
  UIValidationPass:
    sub     AL,48
    push    EAX
    mov     EAX,EBX
    mul     tenTimesMultiply
    mov     EBX,EAX
    pop     EAX
    add     EBX, EAX
    loop    validateUIStringDigits
    jmp     UIValSave

  ;_________________________________________________________________________
  ; - On UI being invalid, print prompt saying so and ask for proper input: |
  ;________________________________________________________________________/
  UIValidationFail:
    mDisplayString  [EBP+24]
    jmp     getUIPrompt

  ;____________________________________________________________________________
  ; - Check input  for being too small or too large via UIValSizeN, UIValSizeP:|
  ;____________________________________________________________________________|
  UIValSave:
    cmp     inputValPorN, 1
    jne      UIValSizeP
  ;_______________________________________________________
  ; - Signed vals are compared to see if they can properly |
  ;  fit, or are less than SDWORD limit                    |
  ;________________________________________________________|
  UIValSizeN:   ; = -2147483648
    cmp     EBX,80000000h
    ja      UIValSizeFail
    neg     EBX
    jmp     UIValSizePass

  ;____________________________
  ; - Check size of unsigned:  |
  ;____________________________|
  UIValSizeP:   ; = -2147483648
    cmp     EBX,7FFFFFFFh
    ja      UIValSizeFail
    jmp     UIValSizePass

  ;______________________________________________________________________________________
  ; - Input doesnt fit within 32 bit reg, let user know then prompt for new input value: \
  ;______________________________________________________________________________________/  
  UIValSizeFail:
     mDisplayString  [EBP+24]
     jmp     getUIPrompt

  ;___________________________________________________________________________________
  ; - Input fits within 32 bit reg. index will point to inputval and is then stored. /
  ;_________________________________________________________________________________/
  UIValSizePass:
    mov     EDI,[EBP+8]
    mov     [EDI],EBX
    popad
    ret     30

ReadVal     endp

;__|writeVal|______________________________________________________________________
; writeVal PROC used in conversion of numerical input to string of digits.         \
;  - Input parameters:                                                              \
;    - SDWORD                                                                        |
;    - Memory address of UIStringLengthCount to store SDWORD                         |
;    - Length of value held in UIStringLengthCount                                   ||
;  - Output parameters:                                                               |
;    - Will invoke the mDisplayString macro to print the ascii representation of     |
;    the SDWORD value as output.                                                    /
;__________________________________________________________________________________/
WriteVal    PROC
    LOCAL inputValPorN:DWORD,  divideByTen:DWORD   ; P = non signed int =  0   ||  N = neg, signed int = 1
    ; = 10, for avg       
    pushad
    ;__________________________________________________________________________________
    ; -Below code contains component of wrieVal proc for managing how and where         \
    ; string primitives are to be dealt with during and after conversion of the          |
    ; SDWORD input to it's ascii equivalent:                                             |
    ; - inputValPorN is initally set for an unsigned input val. Then, the starting point |
    ; of arr holding the string is stored/accessed via stringInputVal.                   |
    ; From this starting address, the index is set to the terminal position of array     |
    ; and our loop is made to run in reverse, making i = i - 1.                          |
    ; -Stosb is used to copy the value to be continuously divided by ten from EDI to AL. |
    ;  any remiander values are stored using EDX.                                        |
    ; -Single digit reaminders are converted into ascii by adding 48 post divsion in the |
    ;  loop below. If a jump isnt executed, the input val can be declared negative.     |
    ;____________________________________________________________________________________/
    dec     EDI
    mov     inputValPorN,0
    ; - EBP+12 =  address of stringInputVal
    mov     EDI,[EBP+12]
    ; - EBP+8  =  length of stringInputVal
    add     EDI,[EBP+8]
    dec     EDI             
    std
    mov     AL, 0
    stosb
    ; - EBP+16 = SDWORD val of stringInputVal
    mov     EAX,[EBP+16]
    mov     divideByTen,10
    mov     EDX, 0
    add     EAX, 0
    jns     conversionLoop
    mov     inputValPorN,1
    neg     EAX
    ;___________________________________________________________________________
    ; - Converts digit held by EAX into its ascii equal by dividing by ten.     |
    ;  EDX is then cleared and EAX is checked for remaining digits to potentialy|
    ;  run the loop again.                                                      |
    ; - A negative sign is stored via mov AL, 45 to account for signed input.   |
    ;__________________________________________________________________________/
    conversionLoop:
     div    divideByTen
     add     EDX,48
     push    EAX
     mov     AL,dl
     stosb
     pop     EAX
     mov     EDX,0
     cmp     EAX,0
     jne     conversionLoop
   cmp     inputValPorN,0
   je      UIStrDigitOne_IsPos
   mov     AL, 45
   stosb

   UIStrDigitOne_IsPos:
   inc     EDI
   mDisplayString  EDI
 popad

 ret     12
WriteVal     endp

END main

