unit pxvec2d;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses sysutils;


type

  { TVec2d }

  TVec2d = record
      function pprint:string;
      class operator + (a, b: TVec2d):TVec2d;inline;
      class operator - (a, b: TVec2d):TVec2d;inline;
      case byte of
        0: (x,y  :single);
        1: (cmps : array[0..1] of single);
  end;

implementation

{ TVec2d }

function TVec2d.pprint: string;
begin
  result := '( ' +  FloatToStr(x) + ', ' + FloatToStr(y) + ' )'
end;


class operator TVec2d.-(a, b: TVec2d): TVec2d;
begin
  result.x := a.x - b.x;
  result.y := a.y - b.y;
end;

class operator TVec2d.+(a, b: TVec2D): TVec2d;
begin
  result.x := a.x + b.x;
  result.y := a.y + b.y;
end;


end.

