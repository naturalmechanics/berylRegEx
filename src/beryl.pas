unit beryl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, berylEngine, Libs;

type

  { TForm1 }

  TForm1 = class(TForm)
    Analysis: TMemo;
    btnRUN: TButton;
    btnSHW: TButton;
    btnCLR: TButton;
    testInput: TMemo;
    regExInput: TMemo;



    procedure btnRUNClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private

  public

  end;

var
  Form1: TForm1;         
  regExEngine    : berylEngine.regExHandler;


implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);

begin
  regExEngine    := berylEngine.regExHandler.Create;
end;

procedure TForm1.btnRUNClick(Sender: TObject); //---------------------------------// converts the array of lines to array of strings.
var
  instructions   : Array of String; //--------------------------------------------// this is the variable that can be sent to the regEx engine
  i              : Integer;

  berylNameSpace01: berylEngine.regExNameSpace;
begin

  regExEngine.reset();
  instructions   := []; //--------------------------------------------------------// Otherwise the compiler will complain

  SetLength(instructions, regExInput.Lines.Count);
  for i := 0 to regExInput.Lines.Count -1 do
  begin
      instructions[i] := regExInput.Lines[i];
  end; //-------------------------------------------------------------------------// conversion completed

  regExEngine.preprocessCode(instructions); //------------------------------------// remove line breaks, remove terminators, etc

  regExEngine.makeTree_ofInstructions(); //---------------------------------------// make a doubly linked list
  if(regExEngine.hasError) then Analysis.Append('=================='+Chr(13)+'Error: ' + regExEngine.error+Chr(13) + '=================='); //// If there is an error - then print it

  berylNameSpace01 := berylEngine.regExNameSpace.create('ns01');
  if(regExEngine.hasError) then Analysis.Append('=================='+Chr(13)+'Error: ' + regExEngine.error+Chr(13) + '=================='); //// If there is an error - then print it

  setLength(regExEngine.nameSpaces, 1); //----------------------------------------// make space for a default namespace
  regExEngine.nameSpaces[0] := berylNameSpace01; //-------------------------------// set the default namespace

  regExEngine.testString := testInput.Lines[0] ;

  regExEngine.parseTree();
  if(regExEngine.hasError) then Analysis.Append('=================='+Chr(13)+'Error: ' + regExEngine.error+Chr(13) + '=================='); //// If there is an error - then print it



  Analysis.Append(regExEngine.history);


  //regExEngine.loadTestString();
  //regExEngine.run(); //-----------------------------------------------------------// run the engine

end;

end.

