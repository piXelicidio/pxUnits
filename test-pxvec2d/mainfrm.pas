unit mainFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  pxvec2d;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private

  public
    procedure mprint(s:string);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  v1, v2, v : TVec2d;
begin
  v1.x := 10; v1.y := 2;
  v2 := v1;
  v := v1 + v2;
  mprint( v.pprint );
end;

procedure TForm1.mprint(s:string);
begin
  memo1.lines.add(s)
end;

end.

