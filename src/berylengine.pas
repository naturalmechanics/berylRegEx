unit berylEngine;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Character, Variants, Libs,Generics.Collections, Generics.Defaults;


type

  instructionTypes = (
          EX,
          EQ,
          EV,
          ES,
          EC,
          EN,
          ER,
          EY
                     );

  regExModes       = (
          multi,   //-------------------------------------------------------------// this is a combination of multiple instructions, so parse the children
          single,   //-------------------------------------------------------------// this is single, directly address the instruction
          undef
                     );
  regEx_multiModes = (
          multi_or,    //---------------------------------------------------------// Multiple instructions,  or command
          multi_xor,   //---------------------------------------------------------// Multiple instructions, xor command
          multi_and,   //---------------------------------------------------------// Multiple instructions, and command
          multi_oand,  //---------------------------------------------------------// Multiple instructions, ordered and command
          multi_undef
                     );

  TregExInstruction = ^regExInstruction;

  regExInstruction = packed record

    instructionType : instructionTypes;
    reference       : Integer;
    typeCommands    : Array of String;
    genericCommands : Array of String;

    pattern         : String;
    additionalCode  : String;

    code            : Array of String;

    match           : String;
    location        : Integer;
    count           : Integer;
    condition       : Array of Boolean;

    status          : Boolean;

    rawCode         : String;

    prev            : TregExInstruction;
    next            : TregExInstruction;

    prnt            : TregExInstruction;
    chld            : Array of TregExInstruction;

    instMode        : regExModes;
    chldComb        : regEx_multiModes;

    ID              : Integer;
    result          : String;

  end;

  varTypes = (
           b, //------------------------------------------------------------------// Boolean
           d, //------------------------------------------------------------------// double = Float / double
           i, //------------------------------------------------------------------// integer
           s, //------------------------------------------------------------------// String
           u, //------------------------------------------------------------------// Undefined
           v  //------------------------------------------------------------------// Variant = everything
           );

  TRegExVal = ^regExVal;

  regExVal = packed record
    vtp              : varTypes;
    b                : Boolean;
    f                : Double;
    i                : Integer;
    s                : String;
    v                : Variant;

    isar             : Boolean;


    ba               : Array of Boolean;
    fa               : Array of Double;
    ia               : Array of Integer;
    sa               : Array of String;
    va               : Array of Variant;

    name             : String;

  end;

  { regExNameSpace }

  regExNameSpace = class
    public

    vals             : Array of TRegExVal;
    name             : String;

    constructor Create(n:String) ;

    function  pop (n: String) : regExVal;
    procedure push(n: String; val: regExVal);

    function  getV(n: String) : regExVal;
    procedure setV(n: String; val: regExVal);
  end;

  regExResult = packed record
    status           : Boolean;
    ID               : Integer;
    match            : String;
  end;

  { regExHandler }

  regExHandler = class

    public

    fullCommand      : String;  //------------------------------------------------// this is the current command being handled by the engine
                                                                                  // each TregExInstruction has their own rawcode, but when the
                                                                                  // handler works on a node (recall that TregExInstructions are
                                                                                  // in a linked list tree parent <--> child and prevSibling <--> nextSibling)
                                                                                  // then the rawcode is moved to fullCommand
    fullcommandStack : Array of String;


    root             : TregExInstruction;

    error            : String;
    hasError         : Boolean;


    individualInstructions : Array of String;
    mode_ofRegEx     : regExModes;
    mode_ofMulti     : regEx_multiModes;


    runningID        : Integer;
    history          : String;

    nameSpaces       : Array of regExNameSpace;
    names            : Array of String;

    testString       : String;


    constructor Create();
    procedure reset();
    procedure preprocessCode(code: Array of String);

    procedure makeTree_ofInstructions();
    procedure associateDefaultNameSpace(nm : String; ns: regExNameSpace);
    procedure addNameSpace(nm : String; ns: regExNameSpace);
    procedure parseTree();

    function getResult(id: Integer) : regExResult;
    function getResult(id: Array of Integer) : regExResult;


    procedure recognizeAtoms();
    procedure splitAtoms();
    procedure populateCurrNode(curr : TregExInstruction);

    function popInstruction_atBeginning(i : Integer) : String;
    function getSeperator_betweenInstructions(i: Integer) : String;




  end;

  { instructionParser }

  TStringArray = Array of String;

  instructionParser = class
    public
    inst             : TRegExInstruction;

    hasError         : Boolean;
    Error            : String;

    history          : String;



    target           : String;
    modifiedTarget   : String;

    // ------------------- components for capture variables ----------------------//

    match            : String;
    location         : Integer;
    count            : Integer;
    condition        : Array of Boolean;

    matchName        : String;
    locationName     : String;
    countName        : String;
    conditionName    : String;

    hasMatch         : Boolean;
    hasLocation      : Boolean;
    hasCount         : Boolean;
    hasCondition     : Boolean;

    status          : Boolean;


    // ------------------- components for generic commands -----------------------//

    offset           : Integer;
    range            : Integer;
    fence            : Integer;
    retreat          : Integer;
    anchor           : Integer;

    offsetSet        : Boolean;
    rangeSet         : Boolean;
    fenceSet         : Boolean;
    retreatSet       : Boolean;
    anchorSet        : Boolean;


    // ---------------------- components for exact match -------------------------//



    constructor create(ins : TRegExInstruction);
    procedure populateInstruction();
    function getCommands(cm : String) : TStringArray;
    function getCommandKeyword(cm : String) : String;
    function getCommandValue(cm : String) : String;

    procedure parse();

    procedure parseGenericCommands();

    procedure parseTypeCommands_ex();
    procedure runFiniteStateMachine_ex();

    procedure parseTypeCommands_eq();
    procedure runFiniteStateMachine_eq();

    procedure getDefaultVals();
    procedure runAdditionalCode();


  end;

implementation

{ instructionParser }

constructor instructionParser.create (ins : TRegExInstruction); //----------------// constructor. This is called from regExHandler.ParseTree
begin

  inst               := ins;
  Error              := '';
  hasError           := False;

  history            := '';

  offsetSet          := False;
  rangeSet           := False;
  fenceSet           := False;
  retreatSet         := False;
  anchorSet          := False;

  hasMatch           := False;
  hasLocation        := False;
  hasCount           := False;
  hasCondition       := False;

end;

procedure instructionParser.populateInstruction; //-------------------------------// this function will pick up the code,
                                                                                  // split it in 4 segments,
                                                                                  // and populate the parser variables
var
  nInst              : String;
  i                  : Integer;
  j                  : Integer;
  k                  : Integer;

  c                  : Char;
  currStr            : String;


  foundText          : Boolean;

  read1stPair        : Boolean;
  read2ndPair        : Boolean;
  openEncCount       : Integer;
  clseEncCount       : Integer;
  prevChar           : Char;

  ptrnDtls           : Array of String;
  ptrnXtra           : String;
begin

  // ----------------- First, expect a reference, integer ------------------------//
  currStr            := '';
  for i := 1 to Length(inst^.rawCode) do
  begin
    c                := inst^.rawCode[i]; //--------------------------------------// Keep Reading
    if c = ':' then Break; //-----------------------------------------------------// Found colon. Don't add in the  read buffer, just break. So read head position now points at the colon
    currStr          := currStr + c; //-------------------------------------------// If in this line, then did not break. Hence add to read buffer.
  end;


  if (not IsInteger(Trim(currStr))) then //---------------------------------------// IsInteger is defined in the Libs
  begin
    hasError         := True;
    Error            := 'ER-IP-0000 : Supplied Reference ' + currStr + ' isn''t a Number';
    Exit;
  end;
  inst^.reference    := Trim(currStr).ToInteger; //-------------------------------// Reference is set.
                                                                                     { TODO : MAYBE in future use a bigger variable type like int64 }

  history            := history + Chr(10) + 'Found reference: ' + Trim(currStr);





  // ----------------- Second, expect type, commands, etc ------------------------//

  foundText          := False; //-------------------------------------------------// Now, no text is found yet from the types , commands, patterns, etc...
  currStr            := '';    //-------------------------------------------------// Read buffer is reset.


  for j := i+1 to Length(inst^.rawCode) do //-------------------------------------// currently, i is pointing at ':'. Reject it. hence i+1
                                                                                  // Next item is the type such as EX, this will terminate at a white space
                                                                                  // therefore, the search till next blank space may fail, if we start at the ':'
  begin
    c                := inst^.rawCode[j]; //--------------------------------------// Keep Reading.

    if (not (
          ( c = ' ') or
          ( c = Chr(9)) or
          ( c = Chr(10)) or
          ( c = Chr(13)) or
          ( c = '{') or
          ( c = ':')
       )) then //-----------------------------------------------------------------// first item that is not a blank space.
       begin
         foundText   := True; //--------------------------------------------------// Officially started reading
         currStr     := currStr + c;
       end;
    if (
                                                                                  // We do NOT break at a space. The space are read
                                                                                  // this makes life easier.
                                                                                  // We do not have to continue reading the spaces
                                                                                  // in the next part
                                                                                  // In a normal case, we expect that
                                                                                  // The program reads something other than whitespace
                                                                                  // then either encounters an opening brace
                                                                                  // (if type commands are specified)
                                                                                  // or encounters a colon (no type or generic commands)
          ( c = ':') or //--------------------------------------------------------// Already found the second colon before any {}.
                                                                                  // That means
                                                                                  // No type commands or generic commands.
                                                                                  // or
          ( c = '{') //-----------------------------------------------------------// Found the open enclosure for type Commands
                                                                                  // and
       ) and foundText then //----------------------------------------------------// Has read something that is not whitespace
          begin
            break; //-------------------------------------------------------------// No need to read any more.
                                                                                  // The already read string is the type
          end;
  end;

  case Trim(currStr) of  //-------------------------------------------------------// set the enum for the type
       'EXACT' :
       'EX' : inst^.instructionType := instructionTypes.EX;
       'EQ' : inst^.instructionType := instructionTypes.EQ;
       'EC' : inst^.instructionType := instructionTypes.EC;
       'EN' : inst^.instructionType := instructionTypes.EN;
       'ER' : inst^.instructionType := instructionTypes.ER;
       'EV' : inst^.instructionType := instructionTypes.EV;
       'ES' : inst^.instructionType := instructionTypes.ES;
       'EY' : inst^.instructionType := instructionTypes.EY;
       else
         hasError    := True;
         Error       := 'ER-IP-0001 : Unrecognized Instruction type : ' + Trim(currStr);
         Exit;

  end;

  WriteStr(nInst, inst^.instructionType);
  history            := history + Chr(10) + 'Found Type: ' + Trim(currStr);





  // -- two and a half, if not a colon, then pick up type and generic commands ---//

  if c = '{' then //--------------------------------------------------------------// Type commands are only expected if we enconter a opening brace
  begin
    currStr          := '';
    read1stPair      := False;
    read2ndPair      := False;

    openEncCount     := 0;
    clseEncCount     := 0;

    prevChar         := Chr(0);

    for i := j to Length(inst^.rawCode) do //-------------------------------------// start reading from the opening brace.
    begin
      c              := inst^.rawCode[i];


      if (c = ':') and (prevChar <> '\') and (Trim(currStr) =  '') and (clseEncCount = openEncCount) then Break; // empty braces
      if (c = ':') and (prevChar <> '\') and (Trim(currStr) <> '') then //--------// something found, the termination arrived. BUT braces not closed.
      begin
        hasError     := True;
        Error        := 'ER-IP-0002 : Command Error after regEx type declaration ' + nInst + '. Check both Type and Generic Commands';
        Exit;
      end;

      if c = '{' then openEncCount := openEncCount + 1;
      if c = '}' then clseEncCount := clseEncCount + 1;

      currStr        := currStr + c;
      prevChar       := c;

      if (clseEncCount = openEncCount) and (not read1stPair) and (not read2ndPair) and (Trim(currStr) <> '') then
      begin
        read1stPair  := True;
        history      := history + Chr(10) + 'Type Commands';
        inst^.typeCommands := getCommands(Trim(currStr));
        currStr      := '';
        openEncCount := 0;
        clseEncCount := 0;
      end;

      if (clseEncCount = openEncCount) and (read1stPair) and (not read2ndPair) and (Trim(currStr) <> '') then
      begin
        read2ndPair  := True;     
        history      := history + Chr(10) + 'Generic Commands';
        inst^.genericCommands := getCommands(Trim(currStr));
        currStr      := '';
      end;


    end;
  end;


  // --------------------- Third, pick up the pattern ----------------------------//

  currStr            := '';
  read1stPair        := False;
  read2ndPair        := False;

  openEncCount       := 0;
  clseEncCount       := 0;    

  prevChar           := Chr(0);

  for j := i + 1 to Length(inst^.rawCode) do
  begin
    c                := inst^.rawCode[j];

    if (c = ':') and (prevChar <> '\') and (clseEncCount = openEncCount) then Break;

    if c = '{' then openEncCount := openEncCount + 1;
    if c = '}' then clseEncCount := clseEncCount + 1;

    currStr          := currStr + c;
    prevChar         := c;

  end;
  
  ptrnDtls           := split_byWhiteSpace(Trim(currStr));

  inst^.pattern      := Trim(ptrnDtls[0]);

  WriteStr(nInst, inst^.instructionType);
  history            := history + Chr(10) + 'Found Pattern: ' + Trim(inst^.pattern);

  for k := 1 to Length(ptrnDtls)-1 do
  begin

    ptrnXtra         := Trim(ptrnDtls[k]);

    // ------------------ See if there are match commands ------------------------//
    if (ptrnXtra[1] = '%') then
    begin
      hasMatch       := True;
      matchName      := ptrnXtra;
      history        := history + Chr(10) + 'Found match variable: ' + matchName;
    end;

    // ---------------- See if there are location commands -----------------------//
    if (ptrnXtra[1] = '@') then
    begin
      hasLocation    := True;
      locationName   := ptrnXtra;
      history        := history + Chr(10) + 'Found location variable: ' + locationName;
    end;

    // ----------------- See if there are count commands -------------------------//
    if (ptrnXtra[1] = '_') then
    begin
      hasCount       := True;
      CountName      := ptrnXtra;
      history        := history + Chr(10) + 'Found count variable: ' + countName;
    end;

    // --------------- See if there are condition commands -----------------------//
    if (ptrnXtra[1] = '$') then
    begin
      hasCondition   := True;
      conditionName  := ptrnXtra;
      history        := history + Chr(10) + 'Found match variable: ' + conditionName;
    end;

  end;



  // ------------------- Fourth, pick up the final Code --------------------------//


  currStr            := '';
  read1stPair        := False;
  read2ndPair        := False;

  openEncCount       := 0;
  clseEncCount       := 0;

  prevChar           := Chr(0);

  for i := j + 1 to Length(inst^.rawCode) do
  begin
    c                := inst^.rawCode[i];

    if (c = ';') and (prevChar <> ';') and (clseEncCount = openEncCount) then Break;

    if c = '{' then openEncCount := openEncCount + 1;
    if c = '}' then clseEncCount := clseEncCount + 1;

    currStr          := currStr + c;
    prevChar         := c;

  end;


  inst^.additionalCode:= currStr;



end;

function instructionParser.getCommands(cm: String): TStringArray;
var
  i                  : Integer;
  c                  : Char;
  c_prev             : Char;
  currStr            : String;

  openEnc            : Integer;
  clseEnc            : Integer;
  res                : Array of String;
begin
  cm                 := Trim(cm);

  if not ( (cm[1] = '{') and (cm[Length(cm)] = '}')) then
  begin
    hasError         := True;
    Error            := 'ER-IP-0004 : Encloser mismatch near ' + cm;
    Exit;
  end;

  Delete(cm,1,1);
  Delete(cm,Length(cm),1);

  currStr            := '';

  history            := history + Chr(10) + 'Extracting commands from : ' + cm;
  c                  := Chr(0);
  c_prev             := Chr(0);

  openEnc            := 0;
  clseEnc            := 0;

  setLength(res,0);

  for i := 1 to Length(cm) do
  begin
    c                := cm[i];

    currStr          := currStr + c;

    if (c = '{') and (c_prev <> '\') then openEnc := openEnc + 1;
    if (c = '}') and (c_prev <> '\') then clseEnc := clseEnc + 1;

    if (c = ';') and (c_prev <> '\') and (openEnc = clseEnc) then
    begin
      setLength(res, Length(res) + 1);
      res[Length(res) - 1] := currStr;
      currStr        := '';
      openEnc        := 0;
      clseEnc        := 0;
    end;

    c_prev           := c;
  end;

  Result             := res;

end;

function instructionParser.getCommandKeyword(cm: String): String;
var
  curr               : String;
  c                  : Char;
  i                  : Integer;
begin

  cm                 := Trim(cm);
  curr               := '';

  for i := 1 to Length(cm) do
  begin
    c                := cm[i];
    if isWhiteSpace(c) then Break;
    curr             := curr + c;
  end;

  Result             := Trim(curr);

end;

function instructionParser.getCommandValue(cm: String): String;
var
  curr               : String;
  c                  : Char;
  i                  : Integer;
  j                  : Integer;
begin



  cm                 := Trim(cm);
  curr               := '';

  for i := 1 to Length(cm) do
  begin
    c                := cm[i];
    if isWhiteSpace(c) then Break;
  end;

  for j := i+1 to Length(cm) do
  begin
    curr             := curr + cm[j];
  end;


  curr               := Trim(curr);

  if (curr[Length(curr)] = ';') then Delete(curr, Length(curr),1);
  Result             := Trim(curr);

end;

procedure instructionParser.parse;
begin

  parseGenericCommands;

  case inst^.instructionType of
       instructionTypes.EX:
                      begin

                        parseTypeCommands_ex();

                        runFiniteStateMachine_ex();

                      end;
       instructionTypes.EQ:
                      begin

                      end;
  end;

end;

procedure instructionParser.parseGenericCommands;
var
  i                  : Integer;
  keyword            : String ;
  v                  : String;
begin
  showMessage('parsing Generic Commands : ' );


  for i := 0 to Length(inst^.genericCommands)-1 do
  begin

    keyWord          := getCommandKeyword(inst^.genericCommands[i]);

    case keyWord of
         'OFFSET'    : begin
           v         := getCommandValue(inst^.genericCommands[i]);
           offset    := (v).ToInteger;
           offsetSet := True;
           history   := history + Chr(10) + 'Offset Found : ' + v;
         end;
         'RANGE'     : begin
           v         := getCommandValue(inst^.genericCommands[i]);
           range     := (v).ToInteger;
           rangeSet  := True;
           history   := history + Chr(10) + 'Range Found : ' + v;
         end;
         'FENCE'     : begin
           v         := getCommandValue(inst^.genericCommands[i]);
           fence     := (v).ToInteger;
           fenceSet  := True;
           history   := history + Chr(10) + 'Fence Found : ' + v;
         end;
         'RETREAT'   : begin
           v         := getCommandValue(inst^.genericCommands[i]);
           retreat   := (v).ToInteger;
           retreatSet:= True;
           history   := history + Chr(10) + 'Retreat Found : ' + v;
         end;
         'ANCHOR'    : begin
           v         := getCommandValue(inst^.genericCommands[i]);
           anchor    := (v).ToInteger;
           anchorSet := True;
           history   := history + Chr(10) + 'Anchor Found : ' + v;
         end;
    end;


    if anchorSet and offsetSet then
    begin
      hasError       := True;
      Error          := 'ER-GC-0001 : Anchor and offset can''t work together : ' + inst^.genericCommands[i];
      Exit;
    end;


  end;

end;

procedure instructionParser.parseTypeCommands_ex;
begin

  if Length(inst^.typeCommands) = 0 then Exit;

end;

procedure instructionParser.runFiniteStateMachine_ex; //--------------------------// Exact Match
                                                                                  // This can only accept offset / anchor / retreat / fence / range commands
                                                                                  // everything else is irrelevant
var
  i                  : Integer;
  j                  : Integer;
  fnd                : Boolean;
begin

  // ----------------------------- nothing set -----------------------------------//

  if ( not offsetSet) and (not rangeSet) and (not fenceSet) and (not retreatSet) and (not anchorSet) then
  begin


    fnd              := True;

    for i := 1 to Length(target) - Length(inst^.pattern) do //--------------------// start search at index 1 (strings have index 1) of test string
    begin
      fnd            := True; //--------------------------------------------------// assume found
      for j := 1 to Length(inst^.pattern) do //-----------------------------------// IF pattern is empty, then loop will not run
                                                                                  // but assumtion, that pattern is found will remain true.
      begin
        fnd          := fnd and (target[i+j-1] = inst^.pattern[j]); //------------// check if each character in pattern is found
        if not fnd then break; //-------------------------------------------------// If not found then stop and go try with next index of test string
      end;
      if fnd then Break; //-------------------------------------------------------// if success, then stop searching. if not found, then the outer loop
                                                                                  // (with i) will loop to end. in the last run another check will be
                                                                                  // performed by inner loop (with j)
                                                                                  // if not found, this final check will set fnd to false.
    end;

    status           := fnd; //---------------------------------------------------// Check if found

    if status then
    begin
      match          := inst^.pattern;
      location       := i;

      if hasCount then
      begin
        hasError     := True;
        Error        := 'ER-GN-0000 : count isn''t supported by Exact Match'; //--// Error. don't know what to do.
        Exit;
      end;

      if hasCondition then
      begin
        hasError     := True;
        Error        := 'ER-GN-0001 : condition isn''t supported by Exact Match';
        Exit;
      end;

    end;

    Exit;

  end;


  // ------------------------- only offset is set --------------------------------//

  if (offsetSet) and (not rangeSet) and (not fenceSet) and (not retreatSet) and (not anchorSet) then
  begin

    if offset > Length(target)- Length(inst^.pattern) then //---------------------// Assume the Test String is ABhelloCD. The test string is hello.
                                                                                  // in strings, index is at 1.
                                                                                  // so, if offset is 4, then we start at lloCD, it is still long enough
                                                                                  // to fit hello
                                                                                  // length of ABhelloCD = 9.
                                                                                  // 9 - 5 = 4
                                                                                  // offset can be max length(teststring) - length(pattern)
    begin
      history        := history + Chr(10) + 'WR-GN-0001 : Warning : After offset not sufficient length remains. No match is possible.';
      Exit;
    end;

    fnd              := True;

    if offset < 0 then
    begin
      hasError     := True;
      Error        := 'ER-GN-0002 : offset can''t be less than 0, at : ' + inst^.rawCode;
      Exit;
    end;

    for i := 1 + offset to Length(target) - Length(inst^.pattern) + 1 do //-------// test only between between start index offset + 1 = 5 (remaining : lloCD)
                                                                                  // and start index length(teststring) - length(pattern) + 1 = 4 + 1 = 5 ( also lloCD)
                                                                                  // If we had ABHelloCDE, then (without knowing anything else) the
                                                                                  // pattern could be at the last 5 places (strat index 6, covering 6,7,8,9 an 10th
                                                                                  // character )
                                                                                  // so the upper limit of start index would have been 10-5+1 = 6
    begin
      fnd            := True;
      for j := 1 to Length(inst^.pattern) do
      begin
        fnd          := fnd and (target[i+j-1] = inst^.pattern[j]);
        if not fnd then break;
      end;
      if fnd then Break;
    end;

    status           := fnd;

    if status then
    begin
      match          := inst^.pattern;
      location       := i;

      if hasCount then
      begin
        hasError     := True;
        Error        := 'ER-GN-0000 : count isn''t supported by Exact Match';
        Exit;
      end;

      if hasCondition then
      begin
        hasError     := True;
        Error        := 'ER-GN-0001 : condition isn''t supported by Exact Match';
        Exit;
      end;

    end;

    Exit;

  end;        




  // ---------------------------- only range is set ------------------------------//

  if (not offsetSet) and (rangeSet) and (not fenceSet) and (not retreatSet) and (not anchorSet) then
  begin

    if range <= 0 then
    begin
      hasError       := True;
      Error          := 'ER-GN-0003 : Range is 0 or negative. No match is possible.';
      Exit;
    end;

    if range < Length(inst^.pattern) then
    begin
      hasError       := True;
      Error          := 'ER-GN-0004 : Range is shorter than pattern length. No match is possible.';
      Exit;
    end;

    if range > Length(Target) then
    begin
      history        := history + Chr(10) + 'Range is larger than length of Test String ''' + target + ''' resetting.';
      range          := Length(Target);
    end;

    fnd              := True;

    for i := 1 to range -  Length(inst^.pattern) + 1 do //------------------------// same as below. Just offset = 0.
    begin
      fnd            := True;
      for j := 1 to Length(inst^.pattern) do
      begin
        fnd          := fnd and (target[i+j-1] = inst^.pattern[j]);
        if not fnd then break;
      end;
      if fnd then Break;
    end;

    status           := fnd;

    if status then
    begin
      match          := inst^.pattern;
      location       := i;

      if hasCount then
      begin
        hasError     := True;
        Error        := 'ER-GN-0000 : count isn''t supported by Exact Match';
        Exit;
      end;

      if hasCondition then
      begin
        hasError     := True;
        Error        := 'ER-GN-0001 : condition isn''t supported by Exact Match';
        Exit;
      end;

    end;

    Exit;

  end;



  // -------------------- offset as well as range is set -------------------------//

  if (offsetSet) and (rangeSet) and (not fenceSet) and (not retreatSet) and (not anchorSet) then
  begin

    if offset > Length(target)- Length(inst^.pattern)  then  //-------------------// same as before
    begin
      history        := history + Chr(10) + 'WR-EX-0002 : Warning : offset is larger than length. No match is possible.';
      Exit;
    end;

    if range <= 0 then //---------------------------------------------------------// Range does not work if less than or equal 0
    begin
      hasError       := True;
      Error          := 'ER-GN-0003 : Range is 0 or negative. No match is possible. At ' + inst^.rawCode;;
      Exit;
    end;

    if range < Length(inst^.pattern) then //--------------------------------------// no sufficient search space.
    begin
      hasError       := True;
      Error          := 'ER-GN-0004 : Range is shorter than pattern length. No match is possible. At ' + inst^.rawCode;;
      Exit;
    end;

    if offset < 0 then
    begin
      hasError     := True;
      Error        := 'ER-GN-0002 : offset can''t be less than 0, at : ' + inst^.rawCode;
      Exit;
    end;

    if range > Length(Target) - offset then
    begin
      history        := history + Chr(10) + 'Range is larger than the remaining length of Test String ''' + target + ''' resetting.';
      range          := Length(Target) - offset ;
    end;

    fnd              := True;

    for i := 1 + offset to offset + range - Length(inst^.pattern) + 1 do //-------// again, assume the test string is ABhelloCDEF
                                                                                  // and search string is hello
                                                                                  // offset is, say, 2, so we target the sub string : helloCDEF
                                                                                  // so start index at the beginning of the loop is 3
                                                                                  // (for h, after dropping AB)
                                                                                  // say range is 7 (must be larger than Length of search string
                                                                                  // or equal. here length of search string is 5)
                                                                                  // so, we look up to helloCD (7 more characters)
                                                                                  // up to index 9.  9 = offset + range
                                                                                  // The match must be found within these characters.
                                                                                  // But the outer loop needs to start at the character 'h'
                                                                                  // or maximum at the first 'l'
                                                                                  // helloCD ends at index 9. (index of 'D' is 9)
                                                                                  // 9 - 5 (length of pattern) = 4
                                                                                  // which is the index of 'e'
                                                                                  // but we want the index of first 'l', index is 5 so
                                                                                  // we need 9 - 5 + 1. = offset + range - Length(pattern) + 1
    begin
      fnd            := True;
      for j := 1 to Length(inst^.pattern) do
      begin
        fnd          := fnd and (target[i+j-1] = inst^.pattern[j]);
        if not fnd then break;
      end;
      if fnd then Break;
    end;

    status           := fnd;

    if status then
    begin
      match          := inst^.pattern;
      location       := i;

      if hasCount then
      begin
        hasError     := True;
        Error        := 'ER-GN-0000 : count isn''t supported by Exact Match';
        Exit;
      end;

      if hasCondition then
      begin
        hasError     := True;
        Error        := 'ER-GN-0001 : condition isn''t supported by Exact Match';
        Exit;
      end;

    end;

    Exit;

  end;






  // ---------------------------- only fence is set ------------------------------//

  if (not offsetSet) and (not rangeSet) and (fenceSet) and (not retreatSet) and (not anchorSet) then
  begin

    if fence > Length(target) - Length(inst^.pattern) then
    begin
      history        := history + Chr(10) + 'WR-EX-0002 : Warning : Before fence not sufficient length remains. No match is possible.';
      Exit;
    end;

    if fence < 0 then
    begin
      hasError     := True;
      Error        := 'ER-GN-0005 : fence can''t be less than 0, at : ' + inst^.rawCode;
      Exit;
    end;

    fnd              := True;

    for i := 1 to Length(target) - Length(inst^.pattern) - fence do //------------// reduce by fence
    begin
      fnd            := True;
      for j := 1 to Length(inst^.pattern) do
      begin
        fnd          := fnd and (target[i+j-1] = inst^.pattern[j]);
        if not fnd then break;
      end;
      if fnd then Break;
    end;

    status           := fnd;

    if status then
    begin
      match          := inst^.pattern;
      location       := i;

      if hasCount then
      begin
        hasError     := True;
        Error        := 'ER-GN-0000 : count isn''t supported by Exact Match';
        Exit;
      end;

      if hasCondition then
      begin
        hasError     := True;
        Error        := 'ER-GN-0001 : condition isn''t supported by Exact Match';
        Exit;
      end;

    end;

    Exit;

  end;



  // ------------------------- offset and fence is set ---------------------------//

  if (offsetSet) and (not rangeSet) and (fenceSet) and (not retreatSet) and (not anchorSet) then
  begin

    if fence > Length(target) - Length(inst^.pattern) then
    begin
      history        := history + Chr(10) + 'WR-EX-0002 : Warning : Before fence not sufficient length remains. No match is possible.';
      Exit;
    end;

    if fence < 0 then
    begin
      hasError     := True;
      Error        := 'ER-GN-0005 : fence can''t be less than 0, at : ' + inst^.rawCode;
      Exit;
    end;

    if offset > Length(target)- Length(inst^.pattern) then //---------------------// Assume the Test String is ABhelloCD. The test string is hello.
                                                                                  // in strings, index is at 1.
                                                                                  // so, if offset is 4, then we start at lloCD, it is still long enough
                                                                                  // to fit hello
                                                                                  // length of ABhelloCD = 9.
                                                                                  // 9 - 5 = 4
                                                                                  // offset can be max length(teststring) - length(pattern)
    begin
      history        := history + Chr(10) + 'WR-EX-0001 : Warning : After offset not sufficient length remains. No match is possible.';
      Exit;
    end;

    fnd              := True;

    if offset < 0 then
    begin
      hasError     := True;
      Error        := 'ER-GN-0002 : offset can''t be less than 0, at : ' + inst^.rawCode;
      Exit;
    end;

    if offset > Length(target) - fence  then
    begin
      hasError     := True;
      Error        := 'ER-GN-0006 : offset is beyond Fence : ' + inst^.rawCode;
      Exit;
    end;

    fnd              := True;

    for i := 1 + offset to Length(target) - Length(inst^.pattern) - fence do //---// reduce by fence and add offset
    begin
      fnd            := True;
      for j := 1 to Length(inst^.pattern) do
      begin
        fnd          := fnd and (target[i+j-1] = inst^.pattern[j]);
        if not fnd then break;
      end;
      if fnd then Break;
    end;

    status           := fnd;

    if status then
    begin
      match          := inst^.pattern;
      location       := i;

      if hasCount then
      begin
        hasError     := True;
        Error        := 'ER-GN-0000 : count isn''t supported by Exact Match';
        Exit;
      end;

      if hasCondition then
      begin
        hasError     := True;
        Error        := 'ER-GN-0001 : condition isn''t supported by Exact Match';
        Exit;
      end;

    end;

    Exit;

  end;



  // ------------------------- range and fence is set ----------------------------//

  if (not offsetSet) and (rangeSet) and (fenceSet) and (not retreatSet) and (not anchorSet) then
  begin

    if fence > Length(target) - Length(inst^.pattern) then
    begin
      history        := history + Chr(10) + 'WR-EX-0002 : Warning : Before fence not sufficient length remains. No match is possible.';
      Exit;
    end;

    if fence < 0 then
    begin
      hasError     := True;
      Error        := 'ER-GN-0005 : fence can''t be less than 0, at : ' + inst^.rawCode;
      Exit;
    end;

    if range <= 0 then
    begin
      hasError       := True;
      Error          := 'ER-GN-0003 : Range is 0 or negative. No match is possible.';
      Exit;
    end;

    if range < Length(inst^.pattern) then
    begin
      hasError       := True;
      Error          := 'ER-GN-0004 : Range is shorter than pattern length. No match is possible.';
      Exit;
    end;

    if range > Length(Target) then
    begin
      history        := history + Chr(10) + 'Range is larger than length of Test String ''' + target + ''' resetting.';
      range          := Length(Target);
    end;

    if range > (Length(target) - fence) then
    begin

      history        := history + Chr(10) + 'Range is conflicting with fence : ' + fence + ''' resetting.';
      range          := (Length(target) - fence);

    end;

    fnd              := True;

    for i := 1 to range -  Length(inst^.pattern) + 1 do //------------------------// use range algorithm
    begin
      fnd            := True;
      for j := 1 to Length(inst^.pattern) do
      begin
        fnd          := fnd and (target[i+j-1] = inst^.pattern[j]);
        if not fnd then break;
      end;
      if fnd then Break;
    end;

    status           := fnd;

    if status then
    begin
      match          := inst^.pattern;
      location       := i;

      if hasCount then
      begin
        hasError     := True;
        Error        := 'ER-GN-0000 : count isn''t supported by Exact Match';
        Exit;
      end;

      if hasCondition then
      begin
        hasError     := True;
        Error        := 'ER-GN-0001 : condition isn''t supported by Exact Match';
        Exit;
      end;

    end;

    Exit;

  end;


  // -------------------- offset and range and fence is set ----------------------//

  if (not offsetSet) and (rangeSet) and (fenceSet) and (not retreatSet) and (not anchorSet) then
  begin

    if fence > Length(target) - Length(inst^.pattern) then
    begin
      history        := history + Chr(10) + 'WR-EX-0002 : Warning : Before fence not sufficient length remains. No match is possible.';
      Exit;
    end;

    if fence < 0 then
    begin
      hasError     := True;
      Error        := 'ER-GN-0005 : fence can''t be less than 0, at : ' + inst^.rawCode;
      Exit;
    end;

    if range <= 0 then
    begin
      hasError       := True;
      Error          := 'ER-GN-0003 : Range is 0 or negative. No match is possible.';
      Exit;
    end;

    if range < Length(inst^.pattern) then
    begin
      hasError       := True;
      Error          := 'ER-GN-0004 : Range is shorter than pattern length. No match is possible.';
      Exit;
    end;

    if range > Length(Target) then
    begin
      history        := history + Chr(10) + 'Range is larger than length of Test String ''' + target + ''' resetting.';
      range          := Length(Target);
    end;

    if range > (Length(target) - fence) then
    begin

      history        := history + Chr(10) + 'Range is conflicting with fence : ' + fence + ''' resetting.';
      range          := (Length(target) - fence);

    end;

    if offset > Length(target)- Length(inst^.pattern) then //---------------------// Assume the Test String is ABhelloCD. The test string is hello.
                                                                                  // in strings, index is at 1.
                                                                                  // so, if offset is 4, then we start at lloCD, it is still long enough
                                                                                  // to fit hello
                                                                                  // length of ABhelloCD = 9.
                                                                                  // 9 - 5 = 4
                                                                                  // offset can be max length(teststring) - length(pattern)
    begin
      history        := history + Chr(10) + 'WR-EX-0001 : Warning : After offset not sufficient length remains. No match is possible.';
      Exit;
    end;

    fnd              := True;

    if offset < 0 then
    begin
      hasError     := True;
      Error        := 'ER-GN-0002 : offset can''t be less than 0, at : ' + inst^.rawCode;
      Exit;
    end;

    if range > Length(Target) - offset then
    begin
      history        := history + Chr(10) + 'Range is larger than the remaining length of Test String ''' + target + ''' resetting.';
      range          := Length(Target) - offset ;
    end;

    if offset > Length(target) - fence  then
    begin
      hasError     := True;
      Error        := 'ER-GN-0006 : offset is beyond Fence : ' + inst^.rawCode;
      Exit;
    end;




    fnd              := True;

    for i := 1 + offset to range -  Length(inst^.pattern) + 1 do //---------------// add offset and use range algorithm
    begin
      fnd            := True;
      for j := 1 to Length(inst^.pattern) do
      begin
        fnd          := fnd and (target[i+j-1] = inst^.pattern[j]);
        if not fnd then break;
      end;
      if fnd then Break;
    end;

    status           := fnd;

    if status then
    begin
      match          := inst^.pattern;
      location       := i;

      if hasCount then
      begin
        hasError     := True;
        Error        := 'ER-GN-0000 : count isn''t supported by Exact Match';
        Exit;
      end;

      if hasCondition then
      begin
        hasError     := True;
        Error        := 'ER-GN-0001 : condition isn''t supported by Exact Match';
        Exit;
      end;

    end;

    Exit;

  end;



  hasError           := True;
  Error              := 'ER-GN-0100 : EXACT Parsing failed, because generic command combination is wrong' ;

end;

procedure instructionParser.parseTypeCommands_eq;
begin

end;

procedure instructionParser.runFiniteStateMachine_eq;
begin

end;

procedure instructionParser.getDefaultVals;
begin

end;

procedure instructionParser.runAdditionalCode;
begin

end;

{ regExNameSpace }

constructor regExNameSpace.Create(n: String);
begin
  name               := n;
  SetLength(vals,0);
end;

function regExNameSpace.pop(n: String): regExVal;
begin

end;

procedure regExNameSpace.push(n: String; val: regExVal);
begin

end;

function regExNameSpace.getV(n: String): regExVal;
begin

end;

procedure regExNameSpace.setV(n: String; val: regExVal);
begin

end;


{ regExHandler }

{Call this first}
constructor regExHandler.Create; //-----------------------------------------------// initialize the root
begin
  root               := New(TregExInstruction);  //-------------------------------// Initialize the root node.
                                                                                  // If the first instruction is a no op (EY match)
                                                                                  // then the script goes in here.


  root^.code         := []; //----------------------------------------------------// Nothing
  root^.condition    := []; //----------------------------------------------------// it is not a enquiry / conditional match -->
                                                                                  // set empty
  root^.instructionType:= instructionTypes.EY; //---------------------------------// NO INSTRUCTION
  root^.reference    := 0; //-----------------------------------------------------// reference is set as 0 for now
                                                                                  // later, once we have all other instructions, we will replace
                                                                                  // the reference

  root^.typeCommands := nil;
  root^.genericCommands := nil;
  root^.location     := 0;
  root^.match        := '';
  root^.pattern      := '';   //--------------------------------------------------// Set all segments to nil
  root^.next         := nil;
  root^.prev         := nil;
  SetLength(root^.chld,0);
  root^.prnt         := nil;  //--------------------------------------------------// Set all addresses to nil

  root^.rawCode      := ''; //----------------------------------------------------// NO code
  root^.instMode     := regExModes.undef;
  root^.chldComb     := regEx_multiModes.multi_undef;  //-------------------------// set modes of node to undefined

  hasError           := False;
  Error              := ''; //----------------------------------------------------// Remove errors

  fullCommand        := '';
  setLength(individualInstructions,0); //-----------------------------------------// Remove the currently handled command

  mode_ofRegEx       := regExModes.undef;
  mode_ofMulti       := regEx_multiModes.multi_undef; //--------------------------// set currently handled modes to undefinded

  history            := '';

  root^.ID           := 0;
  runningID          := 0; //-----------------------------------------------------// reset ID

  SetLength(nameSpaces, 0);
  SetLength(names, 0);  //--------------------------------------------------------// Clear namespaces


end;

{Call this second}
procedure regExHandler.reset; //--------------------------------------------------// reser the regEx handler....
begin

  root               := New(TregExInstruction);  //-------------------------------// Initialize the root node.
                                                                                  // If the first instruction is a no op (EY match)
                                                                                  // then the script goes in here.


  root^.code         := []; //----------------------------------------------------// Nothing
  root^.condition    := []; //----------------------------------------------------// it is not a enquiry / conditional match -->
                                                                                  // set empty
  root^.instructionType:= instructionTypes.EY; //---------------------------------// NO INSTRUCTION
  root^.reference    := 0; //-----------------------------------------------------// reference is set as 0 for now
                                                                                  // later, once we have all other instructions, we will replace
                                                                                  // the reference

  root^.typeCommands := nil;
  root^.genericCommands := nil;
  root^.location     := 0;
  root^.match        := '';
  root^.pattern      := '';   //--------------------------------------------------// Set all segments to nil
  root^.next         := nil;
  root^.prev         := nil;
  SetLength(root^.chld,0);
  root^.prnt         := nil;  //--------------------------------------------------// Set all addresses to nil

  root^.rawCode      := ''; //----------------------------------------------------// NO code
  root^.instMode     := regExModes.undef;
  root^.chldComb     := regEx_multiModes.multi_undef;  //-------------------------// set modes of node to undefined

  hasError           := False;
  Error              := '';   //--------------------------------------------------// Error is erased

  fullCommand        := ''; //----------------------------------------------------// Erase the regex commands. fullcommand will have the command
                                                                                  // the regEx engine is handling currently
  setLength(individualInstructions,0); //-----------------------------------------// remove all temporary instructions

  mode_ofRegEx       := regExModes.undef;
  mode_ofMulti       := regEx_multiModes.multi_undef; //--------------------------// Mode is removed

  root^.ID           := 0;
  runningID          := 0;  //----------------------------------------------------// ID is removed

  history            := ''; //----------------------------------------------------// history is reset

  SetLength(nameSpaces, 0);
  SetLength(names, 0);  //--------------------------------------------------------// Clear namespaces

  {TODO : ADD A PROPER GARBAGE COLLECTION}

end;

{Call this third}
procedure regExHandler.preprocessCode(code: array of String); //------------------// Here, the raw regEx command is supplied.
                                                                                  // The raw regex (human input) may be fragmented.
                                                                                  // a single command may be split into multiple parts.
                                                                                  // multiple commands may be in a single line
                                                                                  // Hence all this function deals with that problem.
                                                                                  // Everything is added in a single string.
                                                                                  // newlines are replaced with a blank space.
                                                                                  // multiple whitespace is compressed in one.
var
  fullString         : String;
  i                  : Integer;
begin

  fullString         := ''; //----------------------------------------------------// EMPTY string

  for i := 0 to Length(code) - 1 do
  begin

    fullString       := fullString + ' ' + Trim(code[i]); //----------------------// REMOVING the start and end whitespaces
                                                                                  // adding one space, in case keywords are seperated by newline
                                                                                  // so keyword newline keyword/value is replaced
                                                                                  // by keyword blackspace keyword/value

  end;

  fullCommand        := fullString;

end;

{Call this fourth}

// This will run a while loop.
// While loop starts at a node, say root node.
// --- The while loop will call recognizeAtoms
// ------ recognizeAtoms identifies single instruction, "and" combination or "or" combination
// ------ recognizeAtoms does not recognize, if we have "ordered and" or "xor"
// ------ for example preprocessCode above as set fullcommand = { command A ;; command B ;; }
// ------ fullCommand is recognized as "and" combination, and braces are removed.
// ------ Now fullcommand = commandA ;; command B ;;
// --- Then splitAtoms is called.
// ------ splitAtoms sets the individualInstructions array.
// ------ IndividualInstructions[0] = command A ;;
// ------ IndividualInstructions[1] = command B ;;
// --- Then populateCurrNode is called.
// ------ PopulateCurrNode will create two child nodes, one for each children and fill them up
// ------ It will also connect each child node with its parents and siblings, and vice versa.
// ------ After this function, each paent will have its children updated,
// ------ each child its parent updated, and each sibling its prev and next sibling updated.
// ------ If there is a single command (such as command C ;;) then the current node.rawCode will be filled up
// --- Then if there is a child, then
// ------ Fullcommand will be set as the rawcode of the first child
// ------ while loop will continue with the first child as current node
// --- If no child, then repeat with the next sibling
// --- If no next, then go back to parent and try sibling of parent
// --- If no next sibling of parent then break
// ----------------------------------------------
// --- In addition the pattern will contain the @Position, %Location ... etc
// --- They will be handled in "ParseTree". ParseTree calls instructionParser.populateInstruction
// --- That will identify these things
procedure regExHandler.makeTree_ofInstructions; //--------------------------------// This one will shove things children of root.
                                                                                  // FOR example, given : { instruction 1 ;; && instruction 2 ;; }
                                                                                  // this is a compound, so we need to assosciate them as children of root
                                                                                  // and make them siblings.
var

  i                  : Integer;
  v                  : String;
  curr               : TregExInstruction;
begin

  curr               := New(TregExInstruction); //--------------------------------// Create a new current node
  curr               := root; //--------------------------------------------------// Set as root
                                                                                  // root already has its rawcode in the initialization phase

                                                                                  // Before entering the loop, fullcommand really
                                                                                  // represents EVERYTHING we supplied in the
                                                                                  // regex. See preprocessCode.
  while True do //----------------------------------------------------------------// keep searching
  begin

    //------------- Pass 1 : Fill the current element ----------------------------//

    recognizeAtoms(); //----------------------------------------------------------// recognizes, if it is a multi command or a single command
    history          := history + Chr(10) + 'Found instruction : ' + fullCommand ;
    writeStr(v, mode_ofRegEx);
    history          := history + Chr(10) + 'Found instruction mode : ' + v ;
    writeStr(v, mode_ofMulti);
    history          := history + Chr(10) + 'Found multi instruction combination mode : ' + v ;


    splitAtoms();     //----------------------------------------------------------// if it single, then only the individualInstructions[0] is set and return
                                                                                  // otherwise, add them in the individual instructions
                                                                                  // the fullcommand is split at this point,
                                                                                  // and set inside individualInstructions
                                                                                  // ASSERT : before entering splitatoms, or just after entering splitatoms
                                                                                  // the individualInstructions is set to empty
    populateCurrNode(curr); //----------------------------------------------------// Pupulate the current Node.
                                                                                  // if single, then set the rawcode and exit
                                                                                  // otherwise, the current node is given children --
                                                                                  // and each children is given one item from
                                                                                  // individualInstructions as rawcode (set during splitAtoms).


                                                                                  // None of the conditions below will execute if the
                                                                                  // current node contains a single instruction


    //------------------ Pass 2 : check all Children -----------------------------// This will be a DFS search


    if Length(curr^.chld) <> 0 then //--------------------------------------------// If the above checks resulted in a "multi" command
                                                                                  // then count of children will not be zero
    begin

      curr           := curr^.chld[0]; //-----------------------------------------// pick the first child - now this child should have access
                                                                                  // to the same global variables as root had
                                                                                  // (as well as curr before entering the loop here)
                                                                                  // so we need to adjust the global variables.
                                                                                  // We modify the same variables as we did in preprocessCode,
                                                                                  // and "create", or "reset".
                                                                                  // Only the runningID increment will not change

      SetLength(fullCommandStack, Length(fullCommandStack) + 1); //---------------// make space
      fullCommandStack[Length(fullCommandStack)-1] := fullCommand; //-------------// copy the current full command, so that we can get it back when
                                                                                  // DFS backtracks, we can find what we were working with.

      fullCommand    := Trim(curr^.rawCode); //-----------------------------------// full command is now the split atom saved in the current
                                                                                  // (child of the previous run) node as "rawcode"
                                                                                  // the parents rawcode is "PROCCHLD" ( = process child)


      mode_ofRegEx   :=  regExModes.undef;
      mode_ofMulti   :=  regEx_multiModes.multi_undef;  //------------------------// reset the modes

      Continue; //----------------------------------------------------------------// reenter the loop. there, the new "fullcommand"
                                                                                  // will be re-recognized and re-atomized
    end;

    if (Length(curr^.chld) = 0) and (not (curr^.next = nil)) then  //-------------// If you have no children, but the next sibling
    begin

      SetLength(fullCommandStack, Length(fullCommandStack) - 1);  //--------------// pop the stack

      curr           := curr^.next; //--------------------------------------------// pick next sibling

      SetLength(fullCommandStack, Length(fullCommandStack) + 1);
      fullCommandStack[Length(fullCommandStack)-1] := fullCommand;

      fullCommand    := Trim(curr^.rawCode);

      mode_ofRegEx   :=  regExModes.undef;
      mode_ofMulti   :=  regEx_multiModes.multi_undef;  //------------------------// reset start positions with sibling information and raw codes

      Continue;
    end;


    if (Length(curr^.chld) = 0) and (curr^.next = nil) and (not (curr^.prnt = nil)) and (not ( (curr^.prnt)^.next = nil)) then   // if no child and siblings but uncle
    begin

      SetLength(fullCommandStack, Length(fullCommandStack) - 1); //---------------// pop one, your own command
      curr           := curr^.prnt;

      SetLength(fullCommandStack, Length(fullCommandStack) - 1); //---------------// pop again, parents command
      curr           := curr^.next;  //-------------------------------------------// pick uncle and repeat


      SetLength(fullCommandStack, Length(fullCommandStack) + 1);
      fullCommandStack[Length(fullCommandStack)-1] := fullCommand;

      fullCommand    := Trim(curr^.rawCode);

      mode_ofRegEx   :=  regExModes.undef;
      mode_ofMulti   :=  regEx_multiModes.multi_undef;

      Continue;
    end;


    SetLength(fullCommandStack,0);  //--------------------------------------------// clear stack, nothing worked
    Break; //---------------------------------------------------------------------// break

  end;

end;


{Call this Fifth}
procedure regExHandler.associateDefaultNameSpace(nm: String; ns: regExNameSpace );  // This will set the default namespace, which is the first element
                                                                                    // of the namespace array
begin
  SetLength(nameSpaces, 1);
  nameSpaces[0]      := ns;

end;




{ASSERT the testString is set}
{Call this Sixth}

// This will set the node ID of every regEx Instruction to the reference
// ID was initially set to running ID in populateInstructions function
// now, they are being changed.
// (recall that TregExInstructions are
// in a linked list tree where
// parent <--> child and prevSibling <--> nextSibling)
// (also recall that the the regExInstructionObjects themselves hold
// status, ID, and result attributes

procedure regExHandler.parseTree; //----------------------------------------------// parse the instructions.
                                                                                  // in fourth call, combined instructions (e.g. "and" combinations)
                                                                                  // were replaced by a PROCCHLD instruction.
                                                                                  // If PROCCHLD, then deal with the children
                                                                                  // ELSE solve the instruction.
var
  curr               : TregExInstruction;
  ref                : Integer;
  commandType        : instructionTypes;
  pattern            : String;
  code               : String;
  iParser            : instructionParser;

  rVal               : TRegExVal;
begin

  curr               := New(TregExInstruction); //--------------------------------// Create a new current node
  curr               := root; //--------------------------------------------------// Set as root. Root is alreade a valid regExInstruction object

  while True do
  begin
    if curr^.rawCode = 'PROCCHLD' then //-----------------------------------------// TODO
    begin

    end
    else //-----------------------------------------------------------------------// Single Instruction
    begin
      iParser        := instructionParser.create(curr); //------------------------// Create a parser upon the instruction.
      iParser.inst   := curr; //--------------------------------------------------// THIS IS PROBABLY UNNECESSARY
      iParser.populateInstruction(); //-------------------------------------------// Split each instruction it's four segments
                                                                                  // (control has left regExHandler)
                                                                                  // (and is in the instructionParser now)


      if iParser.hasError then
      begin
        hasError     := True;
        Error        := iParser.Error;
        Exit;
      end;

      history        := history + Chr(10) + iParser.history; //-------------------// Update history and Error by getting the history and error
                                                                                  // by getting the instruction parser history and error
                                                                                  // and merging them with regExHandler history and error
                                                                                  // this is because we are only echoing the regExHandler history and Error
                                                                                  // for uniformity



      iParser.target := testString; //--------------------------------------------// Set search target

      iParser.parse(); //---------------------------------------------------------// ACTUALLY CALL PARSE
                                                                                  // Since iParser is dynamically created and destroyed
                                                                                  // extract the results
                                                                                  // and put them in the Results array

                                                                                  // AT this point, iparser.match
                                                                                  // iparser.location are already populated
                                                                                  // so we can work with them


      if iParser.hasError then
      begin
        hasError     := True;
        Error        := iParser.Error;
        Exit;
      end;
      history        := history + Chr(10) + iParser.history;

      {TODO deal with the results}

      curr^.status   := iParser.status;
      curr^.result   := iParser.match;

      curr^.ID       := iParser.inst^.reference; //-------------------------------// ID Was set to running ID. Is now the reference


                                                                                  // At this point, if count or condition is worngly set
                                                                                  // iParser.hasError will be set
                                                                                  // (in the respective runFiniteStateMachine_XX functions)
                                                                                  // and the program will already have exited.
                                                                                  // so if any of the hasMatch or hasLocation etc
                                                                                  // is true,then they should have been true.
                                                                                  // The default variables should be inserted in
                                                                                  // the default namespace. which is namesSpaces[0]
      if iParser.hasMatch then
      begin
        rVal         := New(TRegExVal);
        rVal^.name   := iParser.matchName; //-------------------------------------// Set Variable Name
        rVal^.vtp    := varTypes.s; //--------------------------------------------// Set variable type
        rVal^.s      := iparser.match; //-----------------------------------------// set variable value
        rVal^.isar   := False; //-------------------------------------------------// Not an array

        SetLength(nameSpaces[0].vals, Length(nameSpaces[0].vals) + 1);
        nameSpaces[0].vals[Length(nameSpaces[0].vals) - 1] := rVal; //------------// insert into the default namespace
      end;

      if iParser.hasLocation then //----------------------------------------------// same story as above.
      begin
        rVal         := New(TRegExVal);
        rVal^.name   := iParser.locationName;
        rVal^.vtp    := varTypes.i;
        rVal^.i      := iparser.location;
        rVal^.isar   := False;

        SetLength(nameSpaces[0].vals, Length(nameSpaces[0].vals) + 1);
        nameSpaces[0].vals[Length(nameSpaces[0].vals) - 1] := rVal;
      end;


      iParser.Free;
    end;
    if Length(curr^.chld) = 0 then break; //--------------------------------------// TEST PURPOSES. WHEN CHILDBEARING NODE PARSING IMPLEMENTED,
                                                                                  // CHANGE THIS

  end;
end;

function regExHandler.getResult(id: Integer): regExResult;
begin

end;

function regExHandler.getResult(id: array of Integer): regExResult;
begin

end;

procedure regExHandler.addNameSpace(nm: String; ns: regExNameSpace);
begin

end;


{HELPER FUNCTIONS}
procedure regExHandler.recognizeAtoms;  //----------------------------------------// recognize, if this is multi or single instruction mode
                                                                                  // The answer is set to mod_ofRegx.
                                                                                  // Normally, at this point we still dont know if we have
                                                                                  // "ordered and" or "xor"
                                                                                  // This function will only affect the "fullCommand"
                                                                                  // and mode_ofRegex. Also a preliminary value for
                                                                                  // mode_ofMulti will be set
var
  v                  : String;
begin

  fullCommand        := Trim(fullCommand); //-------------------------------------// once more, delete blank spaces

  if (( fullCommand[1] = '{' ) and (fullcommand[Length(fullCommand)] = '}')) then // AND combination. Not sure if ordered or not at this point
  begin
    mode_ofRegEx     := regExModes.multi;
    mode_ofMulti     := regEx_multiModes.multi_and;
    Delete(fullCommand,1,1); //---------------------------------------------------// delete the enclosers
    Delete(fullCommand, Length(fullCommand), 1); //-------------------------------// Also at the end
                                                                                  // at theis point { bla;; {bla;; (bla;;)} bla;; }
                                                                                  // becomes bla;; {bla;; (bla;;)} bla;;
    Exit; //----------------------------------------------------------------------// REST of the code will not be visited
  end;

  if (( fullCommand[1] = '{' ) and (fullCommand[Length(fullCommand)] <> '}')) then// First enclosure is { but last one is not }
  begin
    hasError         := True;
    Error            := 'ER-EN-0001 Encloser Imbalance'; //-----------------------// did not match
    Exit;
  end;

  if (( fullCommand[1] <> '{' ) and (fullCommand[Length(fullCommand)] = '}')) then// Last one is } but first one is not {
  begin
    hasError         := True;
    Error            := 'ER-EN-0002 Unexpected Encloser } at position ' + (Length(fullCommand)).ToString ; // unexpected encloser
    Exit;
  end;

  if (( fullCommand[1] = '(' ) and (fullCommand[Length(fullCommand)] = ')')) then // OR combination. not sure if we have xor or not.
  begin
    mode_ofRegEx     := regExModes.multi;
    mode_ofMulti     := regEx_multiModes.multi_or;
    Delete(fullCommand,1,1);
    Delete(fullCommand, Length(fullCommand), 1); //-------------------------------// same story as above, delete enclosures
    Exit;
  end;

  if (( fullCommand[1] = '(' ) and (fullCommand[Length(fullCommand)] <> ')')) then// First enclosure is ( but last one is not )
  begin
    hasError         := True;
    Error            := 'ER-EN-0003 Encloser Imbalance'; //-----------------------// did not match
    Exit;
  end;

  if (( fullCommand[1] <> '(' ) and (fullCommand[Length(fullCommand)] = ')')) then// Last one is ) but first one is not (
  begin
    hasError         := True;
    Error            := 'ER-EN-0004 Unexpected Encloser ) at position ' + (Length(fullCommand)).ToString ; // unexpected encloser
    Exit;
  end;

  if (
        ( fullCommand[1] <> '{' ) and (fullCommand[Length(fullCommand)] <> '}') and // NO enclosures
        ( fullCommand[1] <> '(' ) and (fullCommand[Length(fullCommand)] <> ')')
     ) then mode_ofRegEx  := regExModes.single;


  history            := history + Chr(10) + 'Successfully prepared : ' + fullCommand;
  WriteStr(v,mode_ofRegEx);
  history            := history + Chr(10) + 'Recognized as : ' + v;

end;

procedure regExHandler.splitAtoms; //---------------------------------------------// This function attacks the class variable "fullCommand"
                                                                                  // ans splits it in sections
                                                                                  // This will encounter the following cases
                                                                                  // 1. stuff ;; stuff ;; .....
                                                                                  // 2. stuff ;; & stuff;; & ....
                                                                                  // 3. stuff;; ~ stuff ;; ~ ...
                                                                                  // 4. Stuff;;
                                                                                  // Since we call "recognizeAtoms" before we call this function
                                                                                  // the enclosing () or {} are removed.
                                                                                  // "stuff" can include balanced {} or () or any combination, tho.
                                                                                  // This function will reset the individualInstructions Buffer
                                                                                  // and fill it up with individual "stuff"
                                                                                  // it will also reset the modes based on the & or ~ found
                                                                                  // the & and ~ are called separators.
                                                                                  // However, if case # 4 is encountered, then it will just set the
                                                                                  // individual instructions buffer and exit
var
  tempInst           : String;
  seperator          : String;
  i                  : Integer;
  v                  : String;
begin

  SetLength(individualInstructions,0); //-----------------------------------------// reset the split atoms buffer.

  if ( mode_ofRegEx = regExModes.single ) then //---------------------------------// Single command. Nothing to do.
  begin
    SetLength(individualInstructions,1);
    individualInstructions[0] := fullCommand ; //---------------------------------// {TODO : is this REALLY needed?} {ANSWER YES. THE parser will need it}
    Exit;
  end;

  i                  := 1; //-----------------------------------------------------// initialize loop variable


  while True do //----------------------------------------------------------------// keep extracting instructiosn after the last valid one.
  begin
    tempInst         := popInstruction_atBeginning(i); //-------------------------// get the first fully valid instruction starting at i

    if (Trim(tempInst) = '' ) then Break; //--------------------------------------// if empty / NOOP, break.

    i                := i + Length(tempInst); //----------------------------------// In the next step, we need to update the point to start search from (offset).
                                                                                  // Say in this step, we started at i
                                                                                  // Therefore, in the next step, we need to start after i.
                                                                                  // In fact, we can as well advance also the length of the result we found in this step.
                                                                                  // Whatever we found in this step will not be part of next step in normal parsing anyway.
                                                                                  // We therefore set the total offset = length of last instruction + the last start point.

    seperator        := getSeperator_betweenInstructions(i); //-------------------// The seperator will appear after that. So get the separator.

    case Trim(seperator) of
         '&'         :   //-------------------------------------------------------// if this is found, and the mode is an "or" or "xor" combination, then we have error
                        begin
                          if ( not ((mode_ofMulti = regEx_multiModes.multi_and) or  (mode_ofMulti = regEx_multiModes.multi_oand)) ) then
                          begin
                            hasError := True;
                            Error    := 'ER-SP-0001 separator mismatch after ' + tempInst;
                            Exit;
                          end;

                          mode_ofMulti := regEx_multiModes.multi_oand;  //--------// no error
                        end;
         '~'         :
                        begin //--------------------------------------------------// again, if this is found, and mode is "and" or "ordered and" then error
                          if ( not ((mode_ofMulti = regEx_multiModes.multi_or) or  (mode_ofMulti = regEx_multiModes.multi_xor)) ) then
                          begin
                            hasError := True;
                            Error    := 'ER-SP-0002 separator mismatch after ' + tempInst;
                            Exit;
                          end;
                          mode_ofMulti := regEx_multiModes.multi_xor;
                        end;
         ''          :  mode_ofMulti := mode_ofMulti; //--------------------------// No error

         {
         else
                        hasError := True;
                        Error    := 'ER-SP-0003 separator unknown after ' + tempInst;
                        Exit;
         } //---------------------------------------------------------------------// can't do this, because no way we will know if the
                                                                                  // next character is really error or not
                                                                                  // it can just be the case that there has been
                                                                                  // no whitespace between the ;; of the prev instruction
                                                                                  // and the next valid (non-separator) character

    end;

    i                := i + Length(seperator); //---------------------------------// update search offset as before

    SetLength(individualInstructions,Length(individualInstructions) + 1);
    individualInstructions[Length(individualInstructions) - 1] := Trim(tempInst); // Insert in buffer

    tempInst         := '';  //---------------------------------------------------// DELETE old temp values

  end;

end;

procedure regExHandler.populateCurrNode(curr: TregExInstruction);  //-------------// Everything is saved in individualInstructions.
                                                                                  // FOR EACH instruction, create a child, and insert inside it.
var
child                : TregExInstruction;
i                    : Integer;
v                    : String;
begin

  if mode_ofRegEx = regExModes.single then
  begin
    curr^.rawCode    := fullCommand; //-------------------------------------------// {TODO : can i replace this with individualInstructions[0]?}
    curr^.instMode   := regExModes.single;
    curr^.ID         := runningID;
    runningID        := runningID + 1;

    history          := history + Chr(10) + 'Populating NODE ' + curr^.ID.ToString + ' with instruction ' + individualInstructions[0];

  end
  else
  begin
    curr^.ID         := runningID;
    runningID        := runningID + 1;
    history          := history + Chr(10) + 'Populating children of ' + curr^.ID.ToString ;
    for i := 0 to Length(individualInstructions) - 1 do
    begin

      child          := New(TregExInstruction);
      child^.rawCode := individualInstructions[i];
      child^.prnt    := curr;

      history        := history + Chr(10) + i.ToString + i.ToString + i.ToString + i.ToString + i.ToString + i.ToString + i.ToString + i.ToString + i.ToString + i.ToString;
      history        := history + Chr(10) + 'created child with instruction ' + individualInstructions[i];
      history        := history + Chr(10) + 'Where parent is :  ' + curr^.ID.ToString  ;
      history        := history + Chr(10) + i.ToString + i.ToString + i.ToString + i.ToString + i.ToString + i.ToString + i.ToString + i.ToString + i.ToString + i.ToString;


      if i > 0 then
      begin
        history      := history + Chr(10) + 'setting prev item by : ' + (curr^.chld[Length(curr^.chld) - 1])^.rawCode;
        child^.prev  := curr^.chld[Length(curr^.chld) - 1];
      end;
      if (i > 0) and (i < Length(individualInstructions)) then
      begin
        history      := history + Chr(10) + 'setting next item for : ' + (curr^.chld[Length(curr^.chld) - 1])^.rawCode;
        (curr^.chld[Length(curr^.chld) - 1])^.next:=child;
      end;

      SetLength(curr^.chld,Length(curr^.chld) +1);
      curr^.chld[Length(curr^.chld) - 1] := child;

    end;

    curr^.instMode   := regExModes.multi;
    curr^.chldComb   := mode_ofMulti;
    curr^.rawCode    := 'PROCCHLD';

    WriteStr(v, curr^.instMode);
    history          := history + Chr(10) + 'Now, the regexMode is ' + v;
    WriteStr(v, curr^.chldComb);
    history          := history + Chr(10) + 'multimode is :  ' + v ;

    history          := history + Chr(10) + 'Populating NODE ' + curr^.ID.ToString + ' with instruction ' + curr^.rawCode;

  end;


end;

function regExHandler.popInstruction_atBeginning(i: Integer): String;
var
  openEnc            : Integer;
  clseEnc            : Integer;

  openPar            : Integer;
  clsePar            : Integer;

  numSep             : Integer;

  currChar           : Char;
  prevChar           : Char;

  j                  : Integer;

  inst               : String;

  firstChar          : String;
  blindMode          : Boolean;
  firstFound         : Boolean;


begin

  currChar           := Chr(0);
  prevChar           := Chr(0);

  numSep             := 0;
  openEnc            := 0;
  clseEnc            := 0;
  openPar            := 0;
  clsePar            := 0;

  SetLength(inst, 0);

  firstChar          := Chr(0);

  blindMode          := False;
  firstFound         := False;


  for j := i to Length(fullCommand) do
  begin

    currChar         := fullCommand[j];
    SetLength(inst, Length(inst) + 1);
    inst[Length(inst)] := currChar;

    if ( not ( (currChar = ' ') or
         (currChar = Chr(9)) or
         (currChar = Chr(10)) or
         (currChar = Chr(13))
         ) ) and (not firstFound) then
    begin
      firstChar      := currChar;
      firstFound     := True;
    end;

    if (currChar = '{') then openEnc := openEnc + 1;
    if (currChar = '}') then clseEnc := clseEnc + 1;

    if (currChar = '(') then openPar := openPar + 1;
    if (currChar = ')') then clsePar := clsePar + 1;

    if firstChar = '{' then
    begin
      blindMode      := True;
    end;

    if firstChar = '(' then
    begin
      blindMode      := True;
    end;

    if blindMode and (openEnc = clseEnc) and (openPar = clsePar) then Break;

    if (currChar = ';') and (prevChar = ';') and (numSep = 3) and (openEnc = clseEnc) and (openPar = clsepar) and (not blindMode) then Break;

    if (currChar = ':') and (prevChar <> '\') and (openEnc = clseEnc) and ( not blindMode) then numSep := numSep + 1;

    if (numSep > 3) and (not blindMode) then
    begin
      hasError       := True;
      Error          := 'ER-TR-0001 : Termination (;;) Expected near ' + inst;
      Exit;
    end;

    prevChar         := currChar;



  end;



  if (Length(inst) >= 2 ) and (inst [Length(inst)] <> ';') and (inst [Length(inst) - 1] <> ';') and (not blindMode) then
  begin
    hasError         := True;
    Error            := 'ER-TR-0002 : Termination (;;) Expected after ' + inst;
    Exit;
  end;

  Result             := inst;


end;

function regExHandler.getSeperator_betweenInstructions(i: Integer): String; //----// This function will find out the seperator.
                                                                                  // We expect only & and ~ as seperators.
var
  j                  : Integer;
  foundChar          : Boolean;
  currChar           : Char;
  inst               : String;
begin

  inst               := '';

  for j := i to Length(fullCommand) do
  begin
    currChar         := fullCommand[j];

    if ( (currChar <> ' ') and
         (currChar <> Chr(9)) and
         (currChar <> Chr(10)) and
         (currChar <> Chr(13)) and
         ( (currChar = '&') or (currChar = '~') ) //------------------------------// Not a whitespace, but & or  ~ found.
       ) then
    begin
      SetLength(inst, Length(inst) + 1);
      inst[Length(inst)] := currChar; //------------------------------------------// copy and break
      Result         := inst;
      Break;
    end
    else if ( (currChar <> ' ') and
         (currChar <> Chr(9)) and
         (currChar <> Chr(10)) and
         (currChar <> Chr(13)) and
         ( not ( (currChar = '&') or (currChar = '~') ) ) //----------------------// not a white space, also not as expected
       ) then
    begin
      Result         := inst; //--------------------------------------------------// read, too far, break.
      Break;
    end
    else if ( (currChar = ' ') or
         (currChar <> Chr(9)) or
         (currChar <> Chr(10)) or
         (currChar <> Chr(13))
         ) then  //---------------------------------------------------------------// whitespace
    begin
      SetLength(inst, Length(inst) + 1); //---------------------------------------// keep copying
      inst[Length(inst)] := currChar;
    end;
  end;

end;


end.
