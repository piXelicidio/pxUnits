unit pxFrameRate;

interface
  uses Windows;

Function CalcFrameRate:single;
procedure SetFrameRateInterval( interval :cardinal );

implementation

var
  FRAMERATE_INTERVAL  :cardinal;
  fLastTime, FrameCounter : cardinal;
  FPS :single;

Function CalcFrameRate:single;
var
  currtime :cardinal;
begin
  inc(FrameCounter);
  currtime := GetTickCount;
  if (CurrTime-fLastTime)>FRAMERATE_INTERVAL then
  begin
    FPS:=(FrameCounter*1000)/(CurrTime-fLastTime);
    FrameCounter:=0;
    fLastTime := CurrTime;
  end;
  Result := FPS;
end;

procedure SetFrameRateInterval( interval :cardinal );
begin
  if interval < 30 then interval:=30;
  FRAMERATE_INTERVAL := interval;
end;

begin
  FRAMERATE_INTERVAL := 500;
  FPS := 0;
end.
 