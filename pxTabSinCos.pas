unit pxTabSinCos;
{
  Tablas de Seno y Coseno bien precisas
}

interface
  Const

    PIx2 = 62831;
    PI   = 62831 div 2;
    PIs2 = PI div 2;
    PIs4 = PI div 4;
    PIs6 = PI div 6;

  Var
    SinT :Array[-200..PIx2+200] of Single;
    CosT :Array[-200..PIx2+200] of Single;

implementation
  Var i:integer;

initialization
  For i:=-200 to PIx2+200 do
  begin
    SinT[i]:=sin(i/10000);
    CosT[i]:=cos(i/10000);
  end;

finalization

end.
