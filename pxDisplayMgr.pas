unit pxDisplayMgr;

interface
  uses
    Forms, 
    Dpx, DpxBmp, DpxDDraw, pxDDrawApi;

  Type

  TDisplayMgr = class
  public
    constructor create;
    destructor destroy;override;
  private
    function GetIsFullScreen: boolean;
    function InitDirectDraw: boolean;
    function InitWindowBmp: boolean;
  protected
    fMainForm :TForm;
    fWidth,
    fHeight   :integer;

    fDDraw  :TMyDirectDraw;
    fDpxDDraw :TDpxDDraw;
    fDpxBmp   :TDpxBmp;
    fDpxScreen  :TDpxRef;

    procedure ReleaseDirectDraw;
    procedure ReleaseWindowBmp;
  public
    function Init( MainForm:TForm; aWidth, aHeight:integer; FullScreen:boolean = false):boolean;
    function BeginDrawing:boolean;
    procedure EndDrawing;
    procedure SetFullScreen( value:boolean );
    procedure FlipShow;
    property Screen:TDpxRef read fDpxScreen;
    property IsFullScreen:boolean read GetIsFullScreen;
    property MainForm:TForm read fMainForm;
  end;

implementation

{ TDisplayMgr }

constructor TDisplayMgr.create;
begin
  fDDraw := nil;
  fDpxDDraw := nil;
  fDpxBmp := nil;
  fDpxScreen := TDpxRef.Create;
  fMainForm := nil;
  fWidth := 0;
  fHeight := 0;
end;

destructor TDisplayMgr.destroy;
begin
  if fDpxDDraw<>nil then ReleaseDirectDraw;
  if fDpxBmp<>nil then ReleaseWindowBmp;
  fDpxScreen.Free;
  inherited;
end;

procedure TDisplayMgr.EndDrawing;
begin
  if fDpxDDraw<>nil then fDpxDDraw.EndDrawing;
end;

procedure TDisplayMgr.FlipShow;
begin
  if fDpxDDraw<>nil then
  begin
    //Estoy en modo DirectDraw
    fDDraw.Flip;

  end else if fDpxBmp<>nil then
    Begin
      //Estoy en modo WindowBmp
      fDpxBmp.Show(fMainForm.Canvas.Handle);
    end;

end;

function TDisplayMgr.Init(MainForm: TForm; aWidth, aHeight: integer;
  FullScreen: boolean):boolean;
begin
  fMainForm := MainForm;
  fWidth := aWidth;
  fHeight := aHeight;
  fDpxScreen.InitSize(aWidth, aHeight);
  if FullScreen then
  begin
    Result := InitDirectDraw;
  end else
  begin
    Result := InitWindowBmp;
  end;
end;

function TDisplayMgr.InitDirectDraw: boolean;
begin
  Result := true;
  if fDpxDDraw= nil then                   //Si ya estoy en modo DDraw no hago nada
  begin
    if fDpxBmp<>Nil then                   //Si estaba en modo Bmp darle Release
    begin
      ReleaseWindowBmp;
    end;
    fMainForm.BorderStyle := bsNone;
    fDDraw := TMyDirectDraw.create;
    Result := fDDraw.InitDevice(fMainForm, fWidth, fHeight, 32);

    fDpxDDraw := TDpxDDraw.Create;
    fDpxDDraw.Surface := fDDraw.BackSurface;
  end;
end;

function TDisplayMgr.InitWindowBmp: boolean;
begin
  Result := True;
  if fDpxBmp=nil then
  begin
    if fDpxDDraw<>nil then
    begin
      ReleaseDirectDraw
    end;
    fDpxBmp := TDpxBmp.Create;
    fDpxBmp.ReSize(fWidth, fHeight );
    fMainForm.Width := fWidth;
    fMainForm.Height := fHeight;
    fMainForm.BorderStyle := bsSingle;
    fDpxBmp.CLS(0);
  end;
end;

procedure TDisplayMgr.ReleaseDirectDraw;
begin
  if fDpxDDraw<>nil then fDpxDDraw.Free;
  if fDDraw<>nil then fDDraw.Free;
  fDpxDDraw := nil;
  fDDraw := nil;
end;

procedure TDisplayMgr.ReleaseWindowBmp;
begin
  if fDpxBmp<>nil then fDpxBmp.Free;
  fDpxBmp:=nil;
end;

function TDisplayMgr.BeginDrawing:boolean;
begin
  if fDpxDDraw<>nil then
  begin
    Result := fDpxDDraw.BeginDrawing;
    fDpxScreen.GrabTo(fDpxDDraw, 0,0);
  end else
  begin
    Result:=true;
    if fDpxScreen.RefImage <> fDpxBmp then fDpxScreen.GrabTo(fDpxBmp, 0,0);
  end;
end;

function TDisplayMgr.GetIsFullScreen: boolean;
begin
  Result :=  (fDpxDDraw<>nil );
end;

procedure TDisplayMgr.SetFullScreen(value: boolean);
begin
  if value then InitDirectDraw else InitWindowBmp;
end;

end.
