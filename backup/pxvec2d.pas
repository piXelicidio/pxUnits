unit pxvec2d;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses sysutils;

const
  singleSimilarity = 0.000001;

type

  { TVec2d }

  TVec2d = record
      function pprint:string;
      class operator + (a, b: TVec2d):TVec2d;inline;
      class operator + (a :TVec2d; s :single):TVec2d;inline;overload;
      class operator - (a, b: TVec2d):TVec2d;inline;
      class operator - ( b :TVec2d):TVec2d;inline;
      class operator :=(s :single ):TVec2d;inline;
      class operator :=(a :TVec2d ):string;inline;
      class operator * (a, b: TVec2d):TVec2d;inline;
      class operator * (a :TVec2d; s :single):TVec2d;inline;overload;
      class operator = (a, b :TVec2d ) r :boolean;inline;
      procedure init( ax, ay:single );inline;
      function cross( v2 :TVec2d ):single;inline; //cross product, return Z value.
      function dot( v2 :TVec2d ):single;inline;   //dot product
      function len:single;inline;
      function lenSq:single;inline;
      function lenManhattan:single;inline;
      function normalized:TVec2d;inline;
      procedure normalize;inline;
      procedure rotate( rad :single );inline;
      function rotated( rad :single ):TVec2d;inline;

      //TODO: projections

      case byte of
        0: (x,y  :single);
        1: (cmps : array[0..1] of single);
  end;

  function vec( ax, ay :single ):TVec2d;inline;
  function vecDir( angle : single):TVec2d;inline;

implementation

function vec( ax, ay :single ):TVec2d;
begin
  result.x := ax;
  result.y := ay;
end;

function vecDir(angle: single): TVec2d;
begin
  result.x := cos(angle);
  result.y := sin(angle);
end;

{ TVec2d }

class operator TVec2d.:=(a :TVec2d ):string;inline;
begin
  result := a.pprint;
end;

class operator TVec2d.- ( b :TVec2d):TVec2d;inline;
begin
  result.x := -b.x;
  result.y := -b.y;
end;

procedure TVec2d.rotate( rad :single );inline;
var
  tx :single;
begin
  tx := x;
  x := x * cos(rad) - y * sin(rad);
  y := tx * sin(rad) + y * cos(rad);
end;

function TVec2d.rotated( rad :single ):TVec2d;inline;
begin
  result.x := x * cos(rad) - y * sin(rad);
  result.y := x * sin(rad) + y * cos(rad);
end;


function TVec2d.normalized:TVec2d;inline;
var
  l :single;
begin
  l := len;
  result := vec( x/l, y/l);
end;

procedure TVec2d.normalize;inline;
var
  l :single;
begin
  l := len;
  x :=  x/l;
  y :=  y/l;
end;


function TVec2d.len:single;inline;
begin
  result := sqrt( x*x + y*y );
end;

function TVec2d.lenSq:single;inline;
begin
  result := x*x + y*y;
end;

function TVec2d.lenManhattan:single;inline;
begin
  result := abs(x) + abs(y);
end;

function TVec2d.dot( v2 :TVec2d ):single;inline;
begin
  result := x * v2.x + y * v2.y;
end;

function TVec2d.cross( v2 :TVec2d ):single;inline;
begin
  result := x * v2.y - y * v2.x;
end;

procedure TVec2d.init( ax, ay:single );
begin
  x := ax; y := ay;
end;

function TVec2d.pprint: string;
begin
  result := '(' +  FloatToStr(x) + ',' + FloatToStr(y) + ')'
end;

class operator TVec2d.* (a, b: TVec2d):TVec2d;
begin
  result.x := a.x * b.x;
  result.y := a.y * b.y;
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

class operator TVec2d.+(a :TVec2d; s :single):TVec2d;
begin
  Result.x := a.x + s;
  Result.y := a.y + s;
end;

class operator TVec2d.:=(s :single ):TVec2d;inline;
begin
  result.x := s;
  result.y := s;
end;

class operator TVec2d.= (a, b :TVec2d ) r :boolean;
begin
  r := (abs(a.x - b.x) < singleSimilarity) and (abs(a.y - b.y) < singleSimilarity);
end;

class operator TVec2d.* (a :TVec2d; s :single):TVec2d;inline;overload;
begin
  result.x := a.x * s;
  result.y := a.y * s;
end;

end.

