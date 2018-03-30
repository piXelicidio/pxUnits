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
    procedure FormCreate(Sender: TObject);
  private
    fb :boolean;
  public
    procedure mprint(s:string);
    procedure CheckIf( msg: string; cond: boolean );
    procedure CheckIfVecOp( v1, v2 :TVec2d; op :string; currValue, correctValue :TVec2d );
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  v1, v2, v3 : TVec2d;
  s :string;
begin
  //addintions
  v1 := vec(10,2);
  v2 := vec(5,7);

  CheckIfVecOp( v1, v2, '+', v1 + v2, vec(15,9));
  CheckIfVecOp( v1, v2, '-', v1-v2, vec(5,-5));
  CheckIfVecOp( v1, v2, '*', v1*v2, vec(50,14));

  checkIf( v1.pprint+'* 0.5 ?= (5,1) ', (v1 * 0.5) = vec(5,1) );

  v1 := vec( 10/3, 20/3);
  v2 := vec( 20/6, 40/6);
  CheckIf( v1.pprint + '=' + v2.pprint,  (v1=v2) );

  v1 := vec(1,2);
  v2 := vec(3,0.5);

  CheckIf(v1.pprint +' crossProduct '+v2.pprint +' -> '+ FloatToStr(v1.cross(v2)), v1.cross(v2) = -5.5 );
  CheckIf(v1.pprint +' dotProduct '+v2.pprint+ ' -> '+ FloatToStr(v1.dot(v2)) , v1.dot(v2) = 4 );

  CheckIf(v1.pprint +'.length -> ' + FloatToStr( v1.len ), abs(v1.len - sqrt(5)) < singleSimilarity );
  CheckIf(v1.pprint +'.lengthSquared -> ' + FloatToStr( v1.lenSq ), abs(v1.lenSq - 5) < singleSimilarity );
  CheckIf(v1.pprint +'.lengthManhattan -> ' + FloatToStr( v1.lenManhattan ), abs(v1.lenManhattan - 3) < singleSimilarity );

  //TODO: tests for normalize, rotate...
  v1 := vec(1,0);
  v2 := vec(0,1);
  v3 := v1.rotated(pi/2);
  //v3.rotate(pi/2);
  CheckIf(v1.pprint +'.rotated(pi/2) -> ' + (v1.rotated(pi/2)).pprint, v3 = v2);

  v1 := vec(2, -3);
  CheckIf('-'+ v1.pprint + ' -> ' + (-v1).pprint, -v1 = vec(-2,3) );
  //typecase to string;
  s := v1;
  CheckIf(s, s = '(2,-3)' );
  CheckIf('vec'+v1, ('vec'+v1) = 'vec(2,-3)' );
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  button1.click;
end;

procedure TForm1.mprint(s:string);
begin
  memo1.lines.add(s)
end;

procedure TForm1.CheckIf(msg: string; cond: boolean);
begin
  if cond then
      mprint( msg + ' - Ok')
    else
      mprint( msg + ' -FAIL');
end;

procedure TForm1.CheckIfVecOp(v1, v2: TVec2d; op: string; currValue,
  correctValue: TVec2d);
var
  r :string;
begin
  if currValue=correctValue then r := 'Ok' else r := 'FAIL Correct: '+correctValue.pprint;
  mprint( v1.pprint + op + v2.pprint + ' =? ' + currValue.pprint + ' ' + r );
end;

end.

