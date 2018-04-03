unit DpxDDraw;

interface
  uses
  Dpx, DirectDraw;

type

  TDpxDDraw = class(TDpx)
  public
    constructor Create;
    destructor Destroy;override;
  private
    FSurface  :IDirectDrawSurface;
    FSurfaceData  :pointer;
  protected
  public
    procedure UpdateScanLines;override;
    procedure ReSize(x,y:integer);override;
    property Surface:IDirectDrawSurface read FSurface write FSurface;
    function BeginDrawing:boolean;
    procedure EndDrawing;
  end;

implementation

{ TDirectGraphix }

function TDpxDDraw.BeginDrawing;
var
   SurfaceDesc: TDDSURFACEDESC ;
   hr         : HRESULT;
   i          : integer;
   p          : pointer;
begin
   Result:=true;
   SurfaceDesc.dwSize:=sizeof(SurfaceDesc);
   hr:=Surface.Lock(nil,SurfaceDesc,
                         DDLOCK_WAIT or DDLOCK_SURFACEMEMORYPTR,
                         0);
   if hr=DD_OK then
   begin
      FSurfaceData:=SurfaceDesc.lpSurface;
      FWidth:=SurfaceDesc.dwWidth;
      FHeight:=SurfaceDesc.dwHeight;
      SetClippingArea(0,0, FWidth-1, FHeight-1);
      p:=FSurfaceData;
      SetLength(FScanLines, FHeight);
      if FScanLines[0] <> p then
        for i:=0 to FHeight-1 do
        begin
          FScanLines[i]:=p;
          inc(cardinal(p), SurfaceDesc.lPitch);
        end;
   end else {error}
   begin
    //'Error pidiendo lock');
    Result := false;
   end;
end;

constructor TDpxDDraw.Create;
begin

end;

destructor TDpxDDraw.Destroy;
begin

  inherited;
end;

procedure TDpxDDraw.EndDrawing;
begin
  Surface.Unlock(FSurfaceData)
end;

procedure TDpxDDraw.UpdateScanLines;
begin
  //con las Surfaces de DirectDraw esto é diferente
end;

procedure TDpxDDraw.ReSize(x, y:integer);
begin
  //no se hace resize con las surfaces de directDraw;
end;

end.
