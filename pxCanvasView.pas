unit pxCanvasView;

interface
 uses  Forms, tmVectMat, pxPolygonTools, Graphics, Windows, Types;

 procedure SetCanvasView( cnv:TCanvas );//
 procedure ResetCanvasViewDC( DC:HDC );
 procedure SetCanvasOrigin( o:TVector ); //
 procedure SetCanvasScale( s :single );//
 function GetCanvasScale:single; //
 procedure SetCanvasPan( relative:TVector );//
 function GetCanvasPan:TVector; //
 function  Trans( v:TVector):TPoint;//
 function unTrans( p:TPoint ):TVector; //
 procedure SetCanvasTransMat( mat:TMatrix );
 procedure PutPixel ( v:TVector; c:TColor ); //
 procedure DrawCircle ( center:TVector; radio:single; c:TColor);//
 procedure DrawRectangle( MinBox, MaxBox:TVector; c:TColor);
 procedure DrawPoly ( vs:TVectorArray; c:TColor);
 procedure DrawPolyLine(vs:TVectorArray; c:TColor); //
 procedure DrawLine(v1, v2:TVector; c:TColor);//
 procedure DrawRow(v1, v2:TVector; size:single; c:TColor );
 procedure DrawCross( v:TVector; size:single; vertical:boolean; c:TColor );
 procedure PrintText( v:TVector; s:string; c:TColor);
 procedure StartPanning(x,y:integer);//
 procedure DoPanning(newx, newy:integer); //
 function  Panning:boolean; //
 procedure StopPanning; //


implementation
  var
    CV    :TCanvas;
 scale    :single;
   pan    :TVector;
 origin   :TVector;
ispanning :boolean;
prevPos   :TPoint;
TransMat  :TMatrix;
InvTransMat :TMatrix;

procedure ResetCanvasViewDC( DC:HDC );
begin
  CV.Handle := DC;
end;

procedure SetCanvasTransMat( mat:TMatrix );
begin
  TransMat := mat;
  MatrixInvert(TransMat, InvTransMat)
end;

procedure PrintText( v:TVector; s:string; c:TColor);
var
  p   :TPoint;
begin
  p := Trans(v);
  CV.Brush.Style := bsClear;
  CV.Font.Color := c;
  CV.TextOut(p.x, p.y, s);
end;

procedure DrawCross( v:TVector; size:single; vertical:boolean; c:TColor );
var
  down, side:TVector;
begin
  down := MakeVector( 0, size, 0);
  side := MakeVector(size, 0, 0);
  if vertical then
  begin
    DrawLine( SubVects(v, down), AddVects(v, down), c);
    DrawLine( SubVects(v, side), AddVects(v, side), c);
  end else
  begin
    DrawLine( SubVects(SubVects(v, down), side), AddVects( AddVects(v, down), side), c);
    DrawLine( SubVects(AddVects(v, down), side), AddVects( SubVects(v, down), side), c);
  end;
end;

procedure DrawRow(v1, v2:TVector; size:single; c:TColor );
var
  side, dir
       :TVector;
    up :TVector;
  l,r:TVector;
begin
  up := MakeVector(0,0,1);
  DrawLine(v1, v2, c);
  dir := SubVects(v1, v2);
  side := CrossProd(dir, up);
  if (not EqualVects(side, NullVector) )and(not EqualVects(dir,NullVector)) then
  begin
    Normalize(side);
    RealProd(side, 0.5);
    Normalize(dir);
    l := AddVects(dir, side);
    NegVect(Side);
    r := AddVects(dir, side);
    Normalize(l);
    Normalize(r);
    RealProd(l, size);
    RealProd(r, size);
    DrawLine(v2, AddVects(v2, l), c);
    DrawLine(v2, AddVects(v2, r), c);
  end;
end;

procedure StopPanning;
begin
  ispanning:=false;
end;

function Panning:boolean;
begin
  Result := ispanning;
end;

procedure StartPanning(x,y:integer);
begin
  ispanning:=true;
  prevPos.x:=x;
  prevPos.y:=y;
end;
procedure DoPanning(newx, newy:integer);
var
  xrel  :integer;
  yrel  :integer;
  p     :TVector;
begin
    xrel := newx - PrevPos.X;
    yrel := newy - PrevPos.Y;
    p := GetCanvasPan;
    p.x := p.x + xrel/GetCanvasScale;
    p.y := p.y + yrel/GetCanvasScale;
    SetCanvasPan(p);
    PrevPos.X:=newx;
    PrevPos.Y:=newy;
end;

procedure DrawLine(v1, v2:TVector; c:TColor);
var
  p:TPoint;
begin
  p :=  Trans(v1);
  CV.MoveTo( p.x, p.y);
  p:=   Trans(v2);
  CV.Pen.Color := c;
  CV.LineTo( p.x, p.y);
end;

procedure DrawRectangle( MinBox, MaxBox:TVector; c:TColor);
var
 p1,p2:TPoint;
begin
  cv.Brush.Style := bsClear;
  cv.Pen.Color :=  c;
  p1 := Trans(MinBox);
  p2 := Trans(MaxBox);
  cv.Rectangle(p1.x, p1.y, p2.x, p2.y);
end;

procedure SetCanvasOrigin( o:TVector);
begin
  origin:=o;
end;
function GetCanvasPan:TVector;
begin
  Result:=pan;
end;

function Trans( v:TVector):TPoint;
begin
  v := VectorProdMatrix(TransMat, v);
  v:=AddVects(v, pan);
  RealProd(v, scale);
  v:=AddVects(v, origin);
  Result:=Point(Round(v.x), Round(v.y));
end;

function unTrans( p:TPoint ):TVector;
begin
  Result.x := p.x;
  Result.y := p.y;
  Result.z := 0;
  Result := SubVects(Result, origin);
  RealProd( Result, 1/scale);
  Result := SubVects(Result, pan);
  //Result := VectorProdMatrix(InvTransMat, Result)
end;

procedure SetCanvasView( cnv:TCanvas );
begin
  CV:=cnv;
end;

procedure PutPixel ( v:TVector; c:TColor );
var
  p:TPoint;
begin
  p:=Trans(v);
  CV.Pixels[p.X, p.y] := c;
end;

procedure SetCanvasScale( s:single );
begin
  scale:=s;
end;
function GetCanvasScale:single;
begin
 Result:=scale;
end;

procedure SetCanvasPan( relative:TVector);
begin
  Pan:=Relative;
end;

procedure DrawPolyLine ( vs:TVectorArray; c:TColor);
var
  i:integer;
  p:TPoint;
begin
  CV.Pen.Color:=c;
  p:=Trans(vs[0]);
  CV.MoveTo(p.X, p.Y);
  for i:=0 to Length(vs)-1 do
  begin
    p:=Trans(vs[i]);
    CV.LineTo(p.x, p.y);
  end;
  {//uncomment this for vertex drawing
  For i:=0 to Length(vs)-1 do
  begin
    p:=Trans(vs[i]);
    if i=0 then CV.Pixels[p.x, p.y]:=clRed else
                CV.Pixels[p.x, p.y]:=0;
  end; }
end;

procedure DrawPoly ( vs:TVectorArray; c:TColor);
var
  p:TPoint;
begin
  DrawPolyLine( vs, c);
  p:=Trans(vs[0]);
  CV.LineTo(p.x, p.y);
  {//uncomment this for vertex drawing
  For i:=0 to Length(vs)-1 do
  begin
    p:=Trans(vs[i]);
    if i=0 then CV.Pixels[p.x, p.y]:=clRed else
                CV.Pixels[p.x, p.y]:=0;
  end; }
end;

procedure DrawCircle ( center:TVector; radio:single; c:TColor);
var
  p:TPoint;
  r:integer;
begin
  p := Trans(center);
  r := Round( radio*Scale );
  CV.Brush.Style := bsClear;
  CV.Pen.Color := c;
  CV.Ellipse(p.x-r, p.y-r, p.x+r, p.y+r);
end;

initialization
  scale:=1;
  pan:=MakeVector(0,0,0);
  origin:=MakeVector(0,0,0);
  ispanning:=false;
  IdentityMatrix(TransMat);
  MatrixInvert(TransMat, InvTransMat);
end.
