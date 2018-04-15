unit px.guiso;
{
  Graphical
  User
  Interface for
  SDL2 with
  Object pascal Delphi

  by Denys Almaral
}
interface
uses
  system.Generics.collections,
  sdl2,
  px.sdl;

type

PUIStyle = ^TUIStyle;
TUIStyle = record
  bk :TSDL_Color;
  fg :TSDL_Color;
  hoverBk :TSDL_color;
  hoverFg :TSDL_color;
  activeBk :TSDL_color;
  activeFg :TSDL_color;
  disabledBk :TSDL_color;
  disabledFg :TSDL_color;
end;

TAreaState = ( asNormal, asHover, asActive, asDisabled);

{ TArea is like our TControl  }
CArea = class of TArea;
TArea = class
  type
    TListAreas = TList<TArea>;
    TUIEventMouseButton = reference to procedure(sender:TArea; mEvent:TSDL_MouseButtonEvent );
    TUIEventMouseMove = reference to procedure(sender:TArea; mEvent:TSDL_MouseMotionEvent );
  private
    fState :TAreaState;
    procedure SetPos(const Value: TSDL_Point);
  protected
    fParent :TArea;
    fPapaOwnsMe :boolean;
    fChilds :TListAreas;
    fRect   :TSDL_Rect;   //rects X,Y are in Screen coordinates;
    fLocal  :TSDL_Point;    //Local coordinates;
    fCatchInput :boolean;
    fVisible :boolean;
    fShowHover :boolean;
    fShowActive :boolean;
    fStyle :TUIStyle;
    fCurrBk : TSDL_Color;
    fCurrFg : TSDL_Color;

    procedure updateScreenCoords;
    procedure setRect(x, y, h, w: integer);
    procedure setState( newState :TAreaState );
  public
    OnMouseMove :TUIEventMouseMove;
    OnMouseDown :TUIEventMouseButton;
    OnMouseUp   :TUIEventMouseButton;
    papaOwnsMe :boolean;
    Visible :boolean;
    bkColor :TSDL_Color;
    fgColor :TSDL_Color;
    Name :string;
    constructor create;
    destructor Destroy;override;
    procedure setXY( x,y :integer );
    procedure setWH( w,h :integer );
    function newChild( areaClass : CArea ):TArea;
    procedure AddChild( aArea :TArea );
    procedure draw;virtual;

    function Consume_MouseDown(const mEvent : TSDL_MouseButtonEvent ):boolean;
    procedure doMouseDown(const mEvent : TSDL_MouseButtonEvent );virtual;
    procedure doMouseUp(const mEvent : TSDL_MouseButtonEvent );virtual;
    procedure doMouseMove(const mEvent : TSDL_MouseMotionEvent );virtual;
    property pos : TSDL_Point read fLocal write SetPos;
 end;

TGuisoPanel = class (TArea)
  public
    constructor create;
end;

TGuisoScreen = class( TArea )
  private
  public
    constructor create;
    destructor Destroy;override;
    procedure draw;override;
 end;

 var
  styleDefault, stylePanel :TUIStyle;

implementation



{ TArea }

procedure TArea.AddChild(aArea: TArea);
begin
  fChilds.Add( aArea );
  aArea.fParent := self;
end;

function TArea.Consume_MouseDown(const mEvent: TSDL_MouseButtonEvent): boolean;
var
  i :integer;
  pos :TSDL_Point;
begin
  Result := false;
  if not fVisible then exit;
  //for all of these three mouse events, XY are in the same position of the record union
  pos.x := mEvent.x;
  pos.y := mEvent.y;
  if SDL_PointInRect(@pos, @fRect ) then
  begin
    //is inside ok, but let's see if my childs consume this input:
    //mouse inputs are processed in the reverse order of how the GUI areas are painted.
    for i := fChilds.Count-1 downto 0 do
    begin
      Result := fChilds.List[i].Consume_MouseDown(mEvent);
      if Result then break;
    end;
    //if non of my childs consumed the input then I eat it.
    if (not Result) and (fCatchInput) then
    begin
      Result := true;
      doMouseDown(mEvent);
    end;
  end
end;

constructor TArea.create;
begin
  fParent := nil;
  fChilds := TListAreas.create;
  fRect := sdl.Rect(0,0, 100, 20);
  fCatchInput := true;
  fPapaOwnsMe := true;
  fVisible := true;
  fShowHover := false;
  fShowActive := false;
  fStyle := styleDefault;
end;

destructor TArea.Destroy;
var
  i :integer;
begin
  //kill childs first
  for i := 0 to fChilds.Count-1 do
    if fChilds.List[i].fPapaOwnsMe then fChilds.List[i].Free;
  fChilds.Free;
end;


procedure TArea.doMouseMove(const mEvent: TSDL_MouseMotionEvent);
begin
  if assigned(OnMouseMove) then OnMouseMove(self, mEvent);
end;

procedure TArea.doMouseDown(const mEvent: TSDL_MouseButtonEvent);
begin
  if assigned(OnMouseDown) then OnMouseDown(self, mEvent);
end;

procedure TArea.doMouseUp(const mEvent: TSDL_MouseButtonEvent);
begin
  if assigned(OnMouseUp) then OnMouseUp(self, mEvent);
end;

procedure TArea.draw;
var
  i :integer;
begin
  if fVisible  then
  begin
    //sdl.setColor( guisoStyle.bk );
    SDL_RenderFillRect(sdl.rend, @fRect);
    //TODO: mabe clip the area of the childs here?
    for i := 0 to fChilds.Count - 1 do fChilds.List[i].draw;
  end;
end;

function TArea.newChild(areaClass: CArea): TArea;
begin
  Result := CArea.create;
  Result.fPapaOwnsMe := true;
  Result.fParent := self;
  fChilds.Add(Result);
end;

procedure TArea.SetPos(const Value: TSDL_Point);
begin
  SetXY(Value.x, Value.y);
end;

procedure TArea.setRect(x, y, h, w : integer );
begin
  fRect := sdl.Rect(x,y,h,w);
  updateScreenCoords;
end;

procedure TArea.setState(newState: TAreaState);
begin
  fState := newState;
  case newState of
    asNormal: begin fgColor := fStyle.fg ; bkColor := fStyle.bk end;
    asHover: begin fgColor := fStyle.hoverFg ; bkColor := fStyle.hoverBk end;
    asActive: begin fgColor := fStyle.activeFg ; bkColor := fStyle.activeBk end;
    asDisabled: begin fgColor := fStyle.disabledFg ; bkColor := fStyle.disabledBk end;
  end;
end;

procedure TArea.setWH(w, h: integer);
begin
  fRect.w := w;
  fRect.h := h;
end;

procedure TArea.setXY(x, y: integer);
begin
  fLocal.x := x;
  fLocal.y := y;
  updateScreenCoords;
end;

{
  since UI most time are static, is better to update the screen coords of the childs
  any time its local x,y are changed than recalculate it every time this coords
  are needed to draw on the screen.
}
procedure TArea.updateScreenCoords;
var
  i: Integer;
begin
  if assigned(fParent) then
  begin
    fRect.x := fParent.fRect.x + fLocal.x;
    fRect.y := fParent.fRect.y + fLocal.y;
  end else
  begin
    fRect.x := fLocal.x;
    fRect.y := fLocal.y;
  end;
  for i := 0 to fChilds.Count-1 do fChilds[i].updateScreenCoords;
end;

{ TGuisoScreen }

constructor TGuisoScreen.create;
begin
  inherited;
  setRect(0,0, sdl.pixelWidth, sdl.pixelHeight);
  fCatchInput := False;
end;

destructor TGuisoScreen.Destroy;
begin

  inherited;
end;

procedure TGuisoScreen.draw;
var
  i: Integer;
begin
  for i := 0 to fChilds.Count-1 do fChilds.List[i].draw;
end;

{ TGuisoPanel }

constructor TGuisoPanel.create;
begin
  inherited;
  fStyle := stylePanel;
end;

initialization
  with styleDefault do
  begin
    fg := sdl.color(200,200,200);
    bk := sdl.color(70, 70, 70);
    hoverFg := sdl.color(250, 250, 250);
    hoverBk := sdl.color(100, 100, 100);
    activeFg := sdl.color(255, 255, 255);
    activeBk := sdl.color(140, 140, 255);
    disabledFg := sdl.color(100, 100, 100);
    disabledBk := sdl.color(30, 30, 30);
  end;
  stylePanel := styleDefault;
  with stylePanel do
  begin
    Fg := sdl.color(150, 150, 150);
    Bk := sdl.color(20, 20, 20);
  end;
finalization
end.
