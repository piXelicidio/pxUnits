unit pxVectorMatrix;
{
Trabajo con vectores y matrices - basado en tmVectMat
organizando y renovando los viejos procedures y funcines
ahora todo dentro de una clase para uso global
- como regla de optimización: Dentro de esta clase
  Ningún método debe hacer llamadas a otros
  para hacer calculos simples como suma y resta de vectores;
  estos deben ser HardCoded a no ser que sea razonable
- Añadir trabajo sobre arreglos de vectores y matrices;
}
interface

type

  PVector=^TVector;
  TVector= record
    x, y, z: Single;
  end;

  TVectorArray = array of TVector;

  TPt = record
    x,y:integer;
  end;

  PMatrix = ^TMatrix;
  TMatrix = record      //compatible con TD3DMatrix
    case integer of
    0:(
      _11, _12, _13, _14: Single;
      _21, _22, _23, _24: Single;
      _31, _32, _33, _34: Single;
      _41, _42, _43, _44: Single;
      );
    1:(
        EjeX :TVector; _14c :single;
        EjeY :TVector; _24c :single;
        EjeZ :TVector; _34c :single;
        Desp :TVector; _44c :single;
      );
  end;


  TVectorMatrix = class
  public
    constructor create;
    destructor destroy;override;
  private
    temps  :single;
    tempv  :TVector;
    tempm  :TMatrix;
  protected
  public
    //Making
    function Make( x,y,z :single ):TVector;
    function MakePolar( ang, module:single):TVector;              //coordenadas polares
    //Simple math
    function  Add( const v1, v2 :TVector ):TVector;
    procedure AddMe( var v1:TVector; const v2:TVector);
    function  Sub( const v1, v2 :TVector):TVector;
    procedure SubMe( var v1:TVector; const v2:TVector);
    function  Scale(const v1:TVector; const val:single):TVector;
    procedure ScaleMe(var v1:TVector; const val:single);
    procedure Invert(var v1:TVector );                            // -Vector
    function  Inverted(const v1:TVector ):TVector;
    //multiply
    function  CrossProd(const v1, v2:TVector):TVector;
    function  DotProd(const v1, v2:TVector):single;
    function  Muliply(const v1, v2:TVector):TVector;
    procedure MuliplyMe( var v1:TVector; const v2:TVector);
    //module
    function  Module(const v :TVector ):single;
    function  ModuleSqr(const v :TVector ):single;                 //Modulo al cuadrado (sin raiz)
    function  ModulesFraction(const v1, v2:TVector):single;
    function  AbsSum(const v :TVector ):single;                    //suma absoluta de componentes
    function  MaxComp(const v :TVector ):single;
    procedure Normalize(var v:TVector );
    function  Normalized(const v:TVector):TVector;

    function  IsNull(const v:TVector):boolean;
    function  IsIqual(const v1, v2 :TVector):boolean;

    function  ZRotated(const v :TVector; const ang:single):TVector;
    procedure ZRotate(var v :TVector; const ang:single);
  end;

  const
  NullVector  :TVector = (x:0; y:0; z:0);
  AxisXVector :TVector = (x:1; y:0; z:0);
  AxisYVector :TVector = (x:0; y:1; z:0);
  AxisZVector :TVector = (x:0; y:0; z:1);


var
  vm :TVectorMatrix;

implementation

{ TVectorMatrix }

function TVectorMatrix.AbsSum(const v: TVector): single;
begin
  Result := abs(v.x) + abs(v.y) + abs(v.z);
end;

function TVectorMatrix.Add(const v1, v2: TVector): TVector;
begin
  Result.x:= v1.x+ v2.x;
  Result.y:= v1.y+ v2.y;
  Result.z:= v1.z+ v2.z;
end;

procedure TVectorMatrix.AddMe(var v1: TVector; const v2: TVector);
begin
  v1.x := v1.x + v2.x;
  v1.y := v1.y + v2.y;
  v1.z := v1.z + v2.z;
end;

constructor TVectorMatrix.create;
begin

end;

function TVectorMatrix.CrossProd(const v1, v2: TVector): TVector;
begin
  result.x:= v1.y* v2.z- v1.z* v2.y;
  result.y:= v1.z* v2.x- v1.x* v2.z;
  result.z:= v1.x* v2.y- v1.y* v2.x;
end;

destructor TVectorMatrix.destroy;
begin

  inherited;
end;

function TVectorMatrix.DotProd(const v1, v2: TVector): single;
begin
  result:= v1.x* v2.x+ v1.y* v2.y+ v1.z* v2.z;
end;

procedure TVectorMatrix.Invert(var v1: TVector);
begin
  v1.x := -v1.x;
  v1.y := -v1.y;
  v1.z := -v1.z;
end;

function TVectorMatrix.Inverted(const v1: TVector): TVector;
begin
  Result.x := -v1.x;
  Result.y := -v1.y;
  Result.z := -v1.z;
end;

function TVectorMatrix.IsIqual(const v1, v2: TVector): boolean;
begin
  Result:= (v1.x=v2.x) and (v1.y=v2.y) and (v1.z=v2.z);
end;

function TVectorMatrix.IsNull(const v: TVector): boolean;
begin
    Result:=(v.x=0) and (v.y=0) and (v.z=0);
end;



function TVectorMatrix.Make(x, y, z: single): TVector;
begin
  Result.x :=x; Result.y:=y; Result.z:=z;
end;

function TVectorMatrix.MakePolar(ang, module: single): TVector;
begin
  Result.x:=module*cos(ang);
  Result.y:=module*sin(ang);
  Result.z:=0;
end;

function TVectorMatrix.MaxComp(const v: TVector): single;
begin
  Result := v.x;
  if v.y > Result then Result := v.y;
  if v.z > Result then Result := v.z;
end;

function TVectorMatrix.Module(const v: TVector): single;
begin
    result:= sqrt(sqr(v.x)+ sqr(v.y)+ sqr(v.z));
end;

function TVectorMatrix.ModulesFraction(const v1, v2: TVector): single;
begin
  Result:= sqr(v1.x)+ sqr(v1.y)+ sqr(v1.z);
  Result:= sqrt( Result/(sqr(v1.x)+ sqr(v1.y)+ sqr(v1.z)) );
end;

function TVectorMatrix.ModuleSqr(const v: TVector): single;
begin
  result:= sqr(v.x)+ sqr(v.y)+ sqr(v.z);
end;

function TVectorMatrix.Muliply(const v1, v2: TVector): TVector;
begin
    Result.x := v1.x * v2.x;
    Result.y := v1.y * v2.y;
    Result.z := v1.z * v2.z;
end;

procedure TVectorMatrix.MuliplyMe(var v1: TVector; const v2: TVector);
begin
    v1.x := v1.x * v2.x;
    v1.y := v1.y * v2.y;
    v1.z := v1.z * v2.z;
end;

procedure TVectorMatrix.Normalize(var v: TVector);
begin
  //  RealProd(vect, 1/Module(vect));
  temps := 1 / sqrt(sqr(v.x)+ sqr(v.y)+ sqr(v.z));
  v.x := v.x * temps;
  v.y := v.y * temps;
  v.z := v.z * temps;
end;

function TVectorMatrix.Normalized(const v: TVector): TVector;
begin
  temps := 1 / sqrt(sqr(v.x)+ sqr(v.y)+ sqr(v.z));
  Result.x := v.x * temps;
  Result.y := v.y * temps;
  Result.z := v.z * temps;
end;

function TVectorMatrix.Scale(const v1: TVector;
  const val: single): TVector;
begin
  result.x := v1.x*val;
  result.y := v1.y*val;
  result.z := v1.z*val;
end;

procedure TVectorMatrix.ScaleMe(var v1: TVector; const val: single);
begin
  v1.x := v1.x*val;
  v1.y := v1.y*val;
  v1.z := v1.z*val;
end;

function TVectorMatrix.Sub(const v1, v2: TVector): TVector;
begin
  Result.x:= v1.x- v2.x;
  Result.y:= v1.y- v2.y;
  Result.z:= v1.z- v2.z;
end;

procedure TVectorMatrix.SubMe(var v1: TVector; const v2: TVector);
begin
  v1.x:= v1.x- v2.x;
  v1.y:= v1.y- v2.y;
  v1.z:= v1.z- v2.z;
end;


procedure TVectorMatrix.ZRotate(var v: TVector; const ang: single);
begin
     v.x:= v.x * cos(ang) - v.y * sin(ang);
     v.y:= v.x * sin(ang) + v.y * cos(ang);
end;

function TVectorMatrix.ZRotated(const v: TVector; const ang:single): TVector;
begin
     result.x:= v.x * cos(ang) - v.y * sin(ang);
     result.y:= v.x * sin(ang) + v.y * cos(ang);
end;

initialization
  vm := TVectorMatrix.create;
finalization
  vm.Free;
end.

