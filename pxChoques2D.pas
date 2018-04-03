Unit pxChoques2D;
{
  Calculo de Choque entre circulos 2D
  Deteccion de choque Circulos y Rectangulos


}

Interface

Uses pxVect2D;


const
  {valores para POSPLANE }
  {PLANO DE POSICION DE UN PUNTO CON RESPECTO A UN RECT}
  PL_ARRIBA     =8; {1000}
  PL_ABAJO      =4; {0100}
  PL_DERECHA    =2; {0010}
  PL_IZQUIERDA  =1; {0001}
  PL_ADENTRO    =0;
   {
    casos
     1001 | 1000 | 1010
    ------|------|----- y1
     0001 | 0000 | 0010
    ------|------|----- y2
     0101 | 0100 | 0110
          x1     x2

     bit0  yc < y1
     bit1  yc > y1
     bit2  xc > x2
     bit3  xc < x1
   }


Procedure CalculaChoque2D(m1, m2:real; p1,p2 :TVect2d; Var v1,v2:TVect2D);
Procedure CalculaChoque2D_Rigid(m1:real; p1,p2 :TVect2d; Var v1:TVect2D);
Function  DetectChoqueRect(x1,y1,w1,h1, x2,y2,w2,h2 :integer):boolean;
Function DetectChoqueCirc(v1, v2:TVect2D; r1,r2:real):boolean;overload;
Function DetectChoqueCirc(x1,y1, x2,y2, r1,r2:real):boolean;overload;
Function DetectChoqueCirc(x1,y1, x2,y2, r1,r2:integer):boolean;overload;
Function DetectChoqueCircRect(x1,y1, x2,y2, xc,yc,radio:real;
                              PosPlane:integer):boolean;

Function SeAcercan(pos1, pos2, vel1, vel2:TVect2D):boolean;
Function PlanePos(xc,yc, x1,y1, x2,y2:real):integer;overload;
Function PlanePos(xc,yc, x1,y1, x2,y2:integer):integer;overload;


Function CalculaChoque_Circ_RigidRect(MasaCirc :real;
                               PCirc, Pmin, Pmax:TVect2D;
                               var vel1 :TVect2D;
                               PosPlane :integer):boolean;



implementation



Procedure CalculaChoque2D(m1, m2:real; p1,p2 :TVect2d; Var v1,v2:TVect2D);
{
 .... Por Denys Almaral Rodriquez                  ALRODE
 ..*. Subrutina para el choque entre dos esferas v2.5
 .... m1,m2  Masas de las esferas
 .... p1,p2 posiciones de las esferas se usan los vectores de Vect2D
 .*.. v1,v2 Componentes de las velocidades antes del choque;
 .... Retorna V1  V2
}
Var
   C,H,B1,B2,R1,R2:Tvect2D;
Begin
    {Vector C diferencia de posiciones}
    subv2d(p1,p2,C);

    {Vector H perpendicular a C}
    H.x := C.y;
    H.y := -C.x;

    {Vector B1 proyeccion de V1 sobre C}
    proyectV2d(v1,C,B1);


    {Vector B2 proyeccion de v2 sobre C}
    proyectv2d(v2,C,B2);


    { Choque (unidimencional) entre vectores B1 con B2, resultados en R1 y R2 }
    R2.x := (2 * B1.x * m1 + B2.x * (m2 - m1)) / (m2 + m1);
    R2.y := (2 * B1.y * m1 + B2.y * (m2 - m1)) / (m2 + m1);
    R1.x := B2.x + R2.x - B1.x;
    R1.y := B2.y + R2.y - B1.y;

    {Vector B1 proyeccion de v1 sobre H}
    proyectv2d(v1,H,B1);

    {Vector B2 proyeccion de v2 sobre H}
    proyectv2d(v2,H,B2);


    {Suma de R1+B1 y R2+B2, resultado en V1, y V2 respectivamente}

    sumv2d(R1,B1,V1);
    sumv2d(R2,B2,V2);

end;


FUNCTION DetectChoqueRect(x1,y1,w1,h1, x2,y2,w2,h2 :integer):boolean;
VAR xd,yd : boolean;
BEGIN
    xd:=false;
    yd:=false;

    if ((x1 <=x2) and (x1+w1>=x2)) then xd:=true else
      if ((x1 <=x2+w2) and (x1+w1>=x2+w2)) then xd:=true else
        if ((x1 >=x2) and (x1+w1<=x2+w2)) then xd:=true;

    if ((y1 <=y2) and (y1+h1>=y2)) then yd:=true else
      if ((y1 <=y2+h2) and (y1+h1>=y2+h2)) then  yd:=true else
        if ((y1 >=y2) and (y1+h1<=y2+h2)) then yd:=true;

    Result:=xd and yd;
END;

Function DetectChoqueCirc(x1,y1, x2,y2, r1,r2:real):boolean;
begin
  Result:=( (sqr(x2-x1) + sqr(y2-y1)) < sqr(r1+r2) );
end;

Function DetectChoqueCirc(x1,y1, x2,y2, r1,r2:integer):boolean;
begin
  Result:=( (sqr(x2-x1) + sqr(y2-y1)) < sqr(r1+r2) );
end;


Function  DetectChoqueCirc(v1, v2:TVect2D; r1,r2:real):boolean;
begin
  Result:=( (sqr(v1.x-v2.x) + sqr(v1.y-v2.y)) < sqr(r1+r2) );
end;

{True si los objetos se acercan}
Function SeAcercan(pos1, pos2, vel1, vel2:TVect2D):boolean;
var
  v1,v2,v3:TVect2D;
begin
    SubV2D(pos1, pos2, v1);
    SumV2D(pos1, vel1, v2);
    SumV2D(pos2, vel2, v3);
    SubV2D(v2,v3,v2);
    Result:= ModV2D(v2)<ModV2D(v1);
end;

Function PlanePos(xc,yc, x1,y1, x2,y2:real):integer;
begin
  Result:=0;
  if yc<y1 then Result:=Result or PL_ARRIBA
    else if yc>y2 then Result:=Result or PL_ABAJO;

  if xc<x1 then Result:=Result or PL_IZQUIERDA
    else if xc>x2 then Result:=Result or PL_DERECHA;
end;

Function PlanePos(xc,yc, x1,y1, x2,y2:integer):integer;
begin
  Result:=0;
  if yc<y1 then Result:=Result or PL_ARRIBA
    else if yc>y2 then Result:=Result or PL_ABAJO;

  if xc<x1 then Result:=Result or PL_IZQUIERDA
    else if xc>x2 then Result:=Result or PL_DERECHA;
end;


Function  DetectChoqueCircRect(x1,y1, x2,y2, xc,yc,radio:real;
                               PosPlane:integer):boolean;
var
  choca:boolean;
begin
  choca:=false;
  case PosPlane of
    PL_ARRIBA     :choca:=((yc+radio)>y1);
    PL_ABAJO      :choca:=((yc-radio)<y2);
    PL_IZQUIERDA  :choca:=((xc+radio)>x1);
    PL_DERECHA    :choca:=((xc-radio)<x2);
    PL_ARRIBA OR PL_IZQUIERDA :choca:=DetectChoqueCirc(xc,yc, x1,y1, radio/2, radio/2);
    PL_ARRIBA OR PL_DERECHA   :choca:=DetectChoqueCirc(xc,yc, x2,y1, radio/2, radio/2);
    PL_ABAJO OR PL_IZQUIERDA  :choca:=DetectChoqueCirc(xc,yc, x1,y2, radio/2, radio/2);
    PL_ABAJO OR PL_DERECHA    :choca:=DetectChoqueCirc(xc,yc, x2,y2, radio/2, radio/2);
    PL_ADENTRO    :choca:=true;
  end;
  Result:=choca;
end;



Procedure CalculaChoque2D_Rigid(m1:real; p1,p2 :TVect2d; Var v1:TVect2D);
{
 .... Por Denys Almaral Rodriquez                  ALRODE
 ..*. Subrutina para el choque entre dos esferas v2.5
 CON UNA ESFERA RIGIDA
  m2=infinito
  v2=0;
}
Var
   C,H,B1,B2,R1,R2:Tvect2D;
   v2:TVect2D;
Begin
    {Rigid}
    v2.x:=0;
    v2.y:=0;

    {Vector C diferencia de posiciones}
    subv2d(p1,p2,C);

    {Vector H perpendicular a C}
    H.x := C.y;
    H.y := -C.x;

    {Vector B1 proyeccion de V1 sobre C}
    proyectV2d(v1,C,B1);


    {Vector B2 proyeccion de v2 sobre C}
    proyectv2d(v2,C,B2);


    { Choque (unidimencional) entre vectores B1 con B2, resultados en R1 y R2 }
  {  R2.x := (2 * B1.x * m1 + B2.x * (m2 - m1)) / (m2 + m1);
    R2.y := (2 * B1.y * m1 + B2.y * (m2 - m1)) / (m2 + m1);}
    //para m2 que tiende a infinito
    R2.x := B2.x ;
    R2.y := B2.y ;

    R1.x := B2.x + R2.x - B1.x;
    R1.y := B2.y + R2.y - B1.y;

    {Vector B1 proyeccion de v1 sobre H}
    proyectv2d(v1,H,B1);

    {Vector B2 proyeccion de v2 sobre H}
    proyectv2d(v2,H,B2);


    {Suma de R1+B1 y R2+B2, resultado en V1, y V2 respectivamente}

    sumv2d(R1,B1,V1);
    sumv2d(R2,B2,V2);

end;




Function CalculaChoque_Circ_RigidRect(MasaCirc :real;
                               PCirc, Pmin, Pmax:TVect2D;
                               var vel1 :TVect2D;
                               PosPlane :integer):boolean;
begin
  result:=true;
  case PosPlane of
    PL_ARRIBA     :if Vel1.y>0 then vel1.y:=-vel1.y else result:=false;
    PL_ABAJO      :if Vel1.y<0 then vel1.y:=-vel1.y else result:=false;
    PL_IZQUIERDA  :if Vel1.x>0 then vel1.x:=-vel1.x else result:=false;
    PL_DERECHA    :if vel1.x<0 then vel1.x:=-vel1.x else result:=false;
    PL_ARRIBA OR PL_IZQUIERDA :
      if SeAcercan(PCirc,PMin,vel1,Vect2D(0,0))
        then CalculaChoque2D_Rigid(MasaCirc, PCirc, PMin, vel1)
          else Result:=false;
    PL_ARRIBA OR PL_DERECHA   :
      if SeAcercan(PCirc, Vect2D(PMax.x, PMin.y), vel1, Vect2D(0,0))
        then CalculaChoque2D_Rigid(MasaCirc, PCirc, Vect2D(PMax.x, PMin.y), vel1)
          else Result:=false;
    PL_ABAJO OR PL_IZQUIERDA  :
      if SeAcercan(PCirc, Vect2D(PMin.x, PMax.y), vel1, Vect2D(0,0))
        then CalculaChoque2D_Rigid(MasaCirc, PCirc, Vect2D(PMin.x, PMax.y), vel1)
          else Result:=false;
    PL_ABAJO OR PL_DERECHA    :
      if SeAcercan(PCirc,PMax,vel1,Vect2D(0,0))
        then CalculaChoque2D_Rigid(MasaCirc, PCirc, PMax, vel1)
          else Result:=false;
  end;
end;


end.

