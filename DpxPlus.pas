unit DpxPlus;
{
  Mas procedmientos gráficos para trabajar
  con todos los Dpx.

  Aqui van los procedimientos que se salen
  un poco de la escencia de la Unit Dpx. Cosas que
  no necesitan o no permiten mucha optimización
  en cuanto a Velocidad.
  O sea gráficos más softy.. como los del GDI :)
}
interface
  uses Dpx, pxVectorMatrix;

  type

  {Esta no hereda de depx, si no que la usa}
  TDpxPlus = class
  public
    constructor create;
    destructor destroy;override;
  private
  protected
    fDpx  :TDpx;
  public
    property Dpx:TDpx read fDpx write fDpx;
    procedure Polyline( pts :TPtArray; c:LongWord );
    procedure PolyLineAnti( pts :TPtArray; c:LongWord );
    procedure PolySmooth( pts :TPtArray; c:LongWord; iterations:integer=3; k1 :single = 1/3; k2:single = 1/3 );
    procedure PolySmoothAnti( pts :TPtArray; c:LongWord; iterations:integer=3; k1 :single = 1/3; k2:single = 1/3 );
    function  SmoothPoly(pts: TPtArray; iterations:cardinal; k1, k2: single):TPtArray;
  end;

implementation

{ TDpxPlus }

constructor TDpxPlus.create;
begin

end;

destructor TDpxPlus.destroy;
begin

  inherited;
end;

procedure TDpxPlus.Polyline(pts: TPtArray; c:LongWord);
var
  i:integer;
begin
  for i:=0 to high(pts)-1 do
  begin
    fDpx.Line(pts[i].x, pts[i].y, pts[i+1].x, pts[i+1].y, c);
  end;
end;

procedure TDpxPlus.PolyLineAnti(pts: TPtArray; c: LongWord);
var
  i:integer;
begin
  for i:=0 to high(pts)-1 do
  begin
    fDpx.LineAnti(pts[i].x, pts[i].y, pts[i+1].x, pts[i+1].y, c);
  end;
end;

procedure TDpxPlus.PolySmooth(pts: TPtArray; c: LongWord;
  iterations: integer; k1, k2: single);
begin
  pts := SmoothPoly(pts, iterations, k1, k2);
  PolyLine(pts, c);
end;

procedure TDpxPlus.PolySmoothAnti(pts: TPtArray; c: LongWord;
  iterations: integer; k1, k2: single);
begin
  pts := SmoothPoly(pts, iterations, k1, k2);
  PolyLineAnti(pts, c);
end;

function TDpxPlus.SmoothPoly(pts: TPtArray; iterations:cardinal; k1, k2: single):TPtArray;
var
  vs :array[0..1] of TVectorArray;
  i, ite  :integer;
  P, V    :integer;
  Cant      :integer;
begin
  SetLength( vs[0], Length(Pts) * ( 1 shl iterations {2^i} ) ) ;
  SetLength( vs[1], Length(Pts) * ( 1 shl iterations {2^i} ) ) ;
  for i:=0 to high(pts) do
  begin
    vs[0][i].x := pts[i].x;
    vs[0][i].y := pts[i].y;
    vs[0][i].z := 0;
  end;
  k2 := 1 - k2;
  P:=0;//pa evitar un warning
  Cant := Length(Pts);
  for ite :=0 to iterations-1 do
  begin
    //V y P se alternan los valores de 0 y 1;
    V := ite mod 2;
    P :=(ite+1) mod 2;
    //el primero y el último vertice se mantienen intactos
    vs[P][0] := vs[V][0];
    vs[P][Cant*2-1] := vs[V][cant-1];
    for i:=0 to cant-2 do
    begin
      //calculando los nuevos
      vs[P][i*2+1] := vm.Add( vs[V][i], vm.Scale( vm.Sub( vs[V][i+1], vs[V][i]), k1) );
      vs[P][i*2+2] := vm.Add( vs[V][i], vm.Scale( vm.Sub( vs[V][i+1], vs[V][i]), k2) );
    end;
    Cant := Cant*2;
  end;

  //resultado;
  SetLength(Result, Length(vs[P]));
  for i:=0 to high(Result) do
  begin
    Result[i].x := round(vs[P][i].x);
    Result[i].y := round(vs[P][i].y);
  end;
end;

end.
