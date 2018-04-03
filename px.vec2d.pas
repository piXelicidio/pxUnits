unit px.vec2d;

interface

uses sysutils;

const
  singleSimilarity = 0.000001;

type

  { TVec2d }

  TVec2d = record
        x,y  :single;
      procedure add( ax, ay :single);overload;inline;
      procedure add( v2 : TVec2d );overload;inline;
      procedure sub( v2 : TVec2d );overload;inline;
      procedure scale( s : single ); inline;
      procedure invert;inline; //negate
      function pprint:string;
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
      class operator add(a, b: TVec2d):TVec2d;inline;
      class operator add(a :TVec2d; s :single):TVec2d;inline;
      class operator add(s :string; b :TVec2d):string;
      class operator subtract(a, b: TVec2d):TVec2d;inline;
      class operator negative( b :TVec2d):TVec2d;inline;
      class operator implicit(s :single ):TVec2d;inline;
      class operator implicit(a :TVec2d ):string;
      class operator multiply(a, b: TVec2d):TVec2d;inline;
      class operator multiply(a :TVec2d; s :single):TVec2d;inline;
      class operator equal(a, b :TVec2d ) :boolean;inline;
      //TODO: projections
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

class operator TVec2d.add(s :string; b :TVec2d):string;
begin
  result := s + b.pprint;
end;

class operator TVec2d.implicit(a :TVec2d ):string;
begin
  result := a.pprint;
end;

class operator TVec2d.negative( b :TVec2d):TVec2d;
begin
  result.x := -b.x;
  result.y := -b.y;
end;

procedure TVec2d.rotate( rad :single );
var
  tx :single;
begin
  tx := x;
  x := x * cos(rad) - y * sin(rad);
  y := tx * sin(rad) + y * cos(rad);
end;

function TVec2d.rotated( rad :single ):TVec2d;
begin
  result.x := x * cos(rad) - y * sin(rad);
  result.y := x * sin(rad) + y * cos(rad);
end;


function TVec2d.normalized:TVec2d;
var
  l :single;
begin
  l := len;
  result := vec( x/l, y/l);
end;

procedure TVec2d.normalize;
var
  l :single;
begin
  l := len;
  x :=  x/l;
  y :=  y/l;
end;


function TVec2d.len:single;
begin
  result := sqrt( x*x + y*y );
end;

function TVec2d.lenSq:single;
begin
  result := x*x + y*y;
end;

function TVec2d.lenManhattan:single;
begin
  result := abs(x) + abs(y);
end;

function TVec2d.dot( v2 :TVec2d ):single;
begin
  result := x * v2.x + y * v2.y;
end;

procedure TVec2d.add(ax, ay: single);
begin
  x := x + ax;
  y := y + ay;
end;

procedure TVec2d.add(v2: TVec2d);
begin
  x := x + v2.x;
  y := y + v2.y;
end;

function TVec2d.cross( v2 :TVec2d ):single;
begin
  result := x * v2.y - y * v2.x;
end;

procedure TVec2d.init( ax, ay:single );
begin
  x := ax; y := ay;
end;

procedure TVec2d.invert;
begin

end;

function TVec2d.pprint: string;
begin
  result := '(' +  FloatToStr(x) + ',' + FloatToStr(y) + ')'
end;

class operator TVec2d.multiply(a, b: TVec2d):TVec2d;
begin
  result.x := a.x * b.x;
  result.y := a.y * b.y;
end;

procedure TVec2d.scale(s: single);
begin
  x := x * s;
  y := y * s;
end;

procedure TVec2d.sub(v2: TVec2d);
begin
  x := x - v2.x;
  y := y - v2.y;
end;

class operator TVec2d.subtract(a, b: TVec2d): TVec2d;
begin
  result.x := a.x - b.x;
  result.y := a.y - b.y;
end;

class operator TVec2d.add(a, b: TVec2D): TVec2d;
begin
  result.x := a.x + b.x;
  result.y := a.y + b.y;
end;

class operator TVec2d.add(a :TVec2d; s :single):TVec2d;
begin
  Result.x := a.x + s;
  Result.y := a.y + s;
end;

class operator TVec2d.implicit(s :single ):TVec2d;
begin
  result.x := s;
  result.y := s;
end;

class operator TVec2d.equal(a, b :TVec2d ) :boolean;
begin
  result := (abs(a.x - b.x) < singleSimilarity) and (abs(a.y - b.y) < singleSimilarity);
end;

class operator TVec2d.multiply (a :TVec2d; s :single):TVec2d;
begin
  result.x := a.x * s;
  result.y := a.y * s;
end;

end.

