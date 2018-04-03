unit pxPhysics;

interface
uses tmVectMat;

type
 TElastic = class
  public
    Constructor Create;
  private
    fStableCenter :TVector;
    fStableCenterLast :TVector;
    fMovilPoint   :TVector;
    fF_Elastic    :TVector;
    fF_Friction   :TVector;
    fMass         :single;
    fKElastic     :single;
    fKFriction    :single;  //k viscosa aire aceite * v^2
    fKRozamiento  :single;
    fAditionalForce   :TVector;
  protected
  public
    Vel :TVector;
    Procedure Move(time:single);
    Procedure SetPos( posi:TVector);
    Property KElastic:single read fKElastic write fKElastic;
    Property KFriction:single read fKFriction write fKFriction;     //Viscosa (aire o líguido)
    Property KRozamiento:single read fKRozamiento write fKRozamiento;
    Property Mass:single read fMass write fMass;
    Property StableCenter:TVector read fStableCenter write fStableCenter;
    Property MovilPoint:TVector read fMovilPoint;
    Property AditionalForce:TVector read fAditionalForce write fAditionalForce;
  end;

implementation


{ TElastic }

constructor TElastic.Create;
begin
    fStableCenter :=MakeVector(0,0,0);
    fMovilPoint   :=fStableCenter;
    Vel          :=MakeVector(0,0,0);
    fF_Elastic    :=MakeVector(0,0,0);
    fF_Friction   :=MakeVector(0,0,0);
    fMass         :=1;
    fKFriction    :=0.01;
    fKElastic     :=0.2;
    fAditionalForce := NullVector;
    fKRozamiento  :=0;
end;

procedure TElastic.Move(time: single);
var
  Fuerza  :TVector;
  Aceleracion :TVector;
  delta       :TVector;
  L           :single;
  v :Extended;
  relVel  :TVector;
  StableCenterVel  :TVector;
  Rozamiento :TVector;
begin

  //Calculo Fuerza Elástica;
  fF_Elastic := SubVects(fStableCenter, fMovilPoint);

  RealProd(fF_Elastic, fKElastic);
  
  //Calculo Fricción (depende de la velocidad del punto
  //movil con respecto a la posicion del punto estable)
  StableCenterVel := SubVects(fStableCenter, fStableCenterLast);

  RelVel := SubVects(Vel, StableCenterVel);
  if Not EqualVects(RelVel, NullVector ) then
  begin
    fF_Friction := RelVel;
    Normalize(fF_Friction);
    Rozamiento:=fF_Friction;
    v:= Module(RelVel); v:=-fKFriction*v*v ;
    RealProd(fF_Friction, v) ;
    //Rozamiento constante
    RealProd(Rozamiento, -fKRozamiento);
  end else
  begin
    fF_Friction := NullVector;
    Rozamiento:=NullVector;
  end;
  fStableCenterLast := fStableCenter;

  //Sumatoria de fuerzas
  Fuerza := AddVects(fF_Elastic, fF_Friction);
  Fuerza := AddVects(Fuerza, Rozamiento);
  Fuerza := AddVects(Fuerza, fAditionalForce);
  //Calculo de aceleracion
  Aceleracion := Fuerza;
  RealProd(Aceleracion, time/fMass);
 
  Vel:=AddVects(Vel, Aceleracion);
  delta := Vel;
  RealProd(delta, time);
  fMovilPoint:= AddVects(fMovilPoint, delta);
  //mayor precicion + (a*t^2)/2
  RealProd(Aceleracion, time*0.5);
  fMovilPoint:= AddVects(fMovilPoint, Aceleracion);
    //nueva posicion;
  //recalculo de la velocidad


end;

procedure TElastic.SetPos(posi: TVector);
begin
    fStableCenter :=posi;
    fMovilPoint   :=posi;
    Vel          :=MakeVector(0,0,0);
    fStableCenterLast := Posi;
end;

end.
 