unit pxAStar;

interface

type
  TNodeType = ( ntEmpty,      ntObstacle,   ntEndPoint,
                ntCloseList,  ntOpenList,   ntStartPoint );

  Txy = record x,y:integer end;

  TPathResult = array of Txy;

  PNodeInfo = ^TNodeInfo;
  TNodeInfo = record
    F, G, H     :integer;
    Parent      :Txy;
    BoardPos    :Txy;
    OpenListPos :integer;
  end;

  TBoardInfo = record
    NodeType  : TNodeType;
    NodeCost  : byte;
    Node      : PNodeInfo;
  end;

  TAStar = class
  public
    Constructor create;
    Destructor Destroy; override;
  private
    function GetType(x, y: integer): TNodeType;
    procedure SetType(x, y: integer; const Value: TNodeType);
    function GetCost(x, y: integer): byte;
    procedure SetCost(x, y: integer; const Value: byte);
    procedure SetHeuristicCalc(const Value: integer);
  protected
    fBoard        :array of array of TBoardInfo;
    fBoardWidth,
    fBoardHeight  :integer;
    fDirs         :array of Txy;
    fDiagDirsIdx  :integer;
    fStartPoint   :Txy;
    fEndPoint     :Txy;

    fCloseList      :array of PNodeInfo;
    fCloseListCount :integer;
    fOpenList       :array of PNodeInfo;  //Binary heap
    fOpenListCount  :integer;

    fHeuristicCalc  :integer; //0-Manhatan&Diagonals 1-Manhattan 2-P2P Distance
    fPathQuality    :integer; //Default 10
    fPathResult    :TPathResult;

    procedure AddToOpenList( n :PNodeInfo );
    function GetOpenListMin: PNodeInfo;
    procedure OpenListItemDecressed( ItemPos :integer );
    procedure AddToCloseList( n :PNodeInfo );
    procedure CleanInit;
  public
    Procedure InitBoard( BoardWidth, BoardHeight :integer );
    Procedure SetStartPoint(x,y:integer);
    Procedure SetEndPoint(x,y:integer);

    function FindPath : boolean;
    Property PathResult : TPathResult read fPathResult;

    Property NodeType[x:integer; y:integer]:TNodeType read GetType write SetType;
    Property NodeCost[x:integer; y:integer]:byte read GetCost write SetCost;
    Property Width:integer read fBoardWidth;
    Property Height:integer read fBoardHeight;
    Property StartPoint:Txy read fStartPoint;
    Property EndPoint:Txy read fEndPoint;
    Property HeuristicCalc:integer read fHeuristicCalc write SetHeuristicCalc; //0-Manhatan&Diagonals 1-Manhattan 2-P2P Distance
    Property PathQuality:integer read fPathQuality write fPathQuality;   // default=10 Best&slow, >10 worse&fast
  end;

  function xy(x,y:integer):Txy;

implementation

var
  DiagCostTable :array[0..255] of integer;

function xy(x,y:integer):Txy;
begin
  Result.x := x;
  Result.y := y;
end;

{ TAStar }

procedure TAStar.AddToCloseList(n: PNodeInfo);
begin
  inc(fCloseListCount);
  if fCloseListCount>Length(fCloseList) then SetLength(fCloseList, fCloseListCount + 100);
  fCloseList[ fCloseListCount-1 ] := n;
  fBoard[n.BoardPos.x, n.BoardPos.y].NodeType := ntCloseList;
end;

procedure TAStar.AddToOpenList(n: PNodeInfo);
var
  currPos : integer;
  parentPos : integer;
  curr  : PNodeInfo;
begin
  inc(fOpenListCount);
  if fOpenListCount > Length( fOpenList ) then SetLength( fOpenList, fOpenListCount + 100);
  currPos := fOpenListCount-1;
  fOpenList[ currPos ] := n;
  curr := n;
  parentPos := currPos div 2;
  while (CurrPos>1) and (curr.F < fOpenList[ ParentPos ].F ) do
  begin
      fOpenList[CurrPos] := fOpenList[ ParentPos ];
      fOpenList[CurrPos].OpenListPos := CurrPos;
      fOpenList[ ParentPos ]:=curr;
      CurrPos := ParentPos;
      ParentPos := CurrPos  div 2;
  end;
  curr.OpenListPos := currPos;
  fBoard[curr.BoardPos.x, curr.BoardPos.y].NodeType := ntOpenList;
end;

procedure TAStar.CleanInit;
var
  i:integer;
  posi  :Txy;
begin
  //OpenList
  For i:=1 to fOpenListCount-1 do
  begin
    posi := fOpenList[i].BoardPos;
    fBoard[posi.x, posi.y].Node := nil;
    fBoard[posi.x, posi.y].NodeType := ntEmpty;
    Dispose(fOpenList[i]);
  end;
  fOpenListCount := 1;
  //CloseList
  For i:=0 to fCloseListCount-1 do
  begin
    posi := fCloseList[i].BoardPos;
    fBoard[posi.x, posi.y].Node := nil;
    fBoard[posi.x, posi.y].NodeType := ntEmpty;
    Dispose(fCloseList[i]);
  end;
  fCloseListCount := 0;
  //unmark EndPoint;
  fBoard[fEndPoint.x, fEndPoint.y].NodeType := ntEmpty;
end;

constructor TAStar.create;
begin
  SetLength(fDirs, 8);
  fDirs[0] := xy( 1, 0);
  fDirs[1] := xy( 0, 1);
  fDirs[2] := xy(-1, 0);
  fDirs[3] := xy( 0,-1);
  fDirs[4] := xy( 1, 1);
  fDirs[5] := xy( 1,-1);
  fDirs[6] := xy( -1,-1);
  fDirs[7] := xy(-1, 1);
  fDiagDirsIdx := 4;
  fCloseListCount := 0;
  SetLength( fCloseList, 100);
  fOpenListCount := 1;
  SetLength( fOpenList, 100 );
  fPathQuality := 10;
  InitBoard(10,10);
end;



destructor TAStar.Destroy;
begin
  inherited;

end;

function TAStar.FindPath: boolean;
var
  curr :PNodeInfo;
  currPos :Txy;
  EndPointFound,
  OpenListEmpty :boolean;
  i :integer;
  cost : byte;
  p :Txy;
  n :PNodeInfo;
  DiagCondition :boolean;
  dmin, dmax  :integer;
  outside :boolean;
  pathResultCount :integer;
begin
  SetLength(fPathResult ,0);
  //marking EndPoint
  fBoard[fEndPoint.x, fEndPoint.y].NodeType := ntEndPoint;
  //first Node StartPoint
  //Coje el StartPoint y lo mete en la OpenList binary heap
  curr := new(PNodeInfo);
  curr.BoardPos := fStartPoint;
  curr.Parent := fStartPoint;
  curr.F := 0; curr.G := 0; curr.H := 0;
  fBoard[fStartPoint.x, fStartPoint.y].Node := curr;
  AddToOpenList( curr );
  EndPointFound := False;
  OpenListEmpty := False;
  repeat
    // coje de la Open List el de menor F (dado por el binary heap)
    curr := GetOpenListMin;
    currPos := curr.BoardPos;

    // Lo añade a la CloseList  y luego analiza en todas las direcciones posibles
    AddToCloseList( curr );

    for i:=0 to Length(fDirs)-1 do
      begin
        //determina el costo segun el paso sea recto o diagonal
        if i<fDiagDirsIdx then cost := fBoard[CurrPos.x, CurrPos.y].NodeCost
                          else cost := DiagCostTable[ fBoard[CurrPos.x, CurrPos.y].NodeCost ];

        // trabaja a P como la posicion nueva a Analizar desde el Curr Node
        p := xy( currPos.x + fDirs[i].x,  currPos.y + fDirs[i].y );
        // verifica que no esté fuera del board
        outside :=  (p.x<0) or (p.y<0) or (p.x>=fBoardWidth) or (p.y>=fBoardHeight);
        if not outside then
        begin
        DiagCondition :=  (i>3);
        // Verifica la condicion de no pasar en diagonal Rozando obstáculos.
        if DiagCondition then DiagCondition :=
                   ( (fBoard[ currPos.x + 0, currPos.y + fDirs[i].y ].NodeType = ntObstacle)
                  or (fBoard[ currPos.x + fDirs[i].x, currPos.y + 0 ].NodeType = ntObstacle) );
        end else DiagCondition := false ;

        if Not outside then
        if Not DiagCondition then
        if (fBoard[p.x, p.y].NodeType = ntEmpty)  then
        begin
          // Si es una posición nueva vacía calcula sus valores
          // y lo añade al final a la Open List.
          n := New(PNodeInfo);
          n.Parent := CurrPos;
          //calculate H G F Values.
          //0-Manhatan&Diagonals 1-Manhattan 2-P2P Distance
          case fHeuristicCalc of
            0:begin
                dmin := abs(EndPoint.X - p.X);
                if abs(EndPoint.y - p.y) < dmin then dmin:=abs(EndPoint.y - p.y);
                dmax :=abs(EndPoint.X - p.X);
                if abs(EndPoint.y - p.y) > dmax then dmax:=abs(EndPoint.y - p.y);
                n.H :=  (dmin*DiagCostTable[ fBoard[CurrPos.x, CurrPos.y].NodeCost]
                      + (dmax - dmin)*fBoard[CurrPos.x, CurrPos.y].NodeCost);
              end;
            1: n.H := fBoard[CurrPos.x, CurrPos.y].NodeCost * (abs(EndPoint.X - p.X) + abs(EndPoint.y - p.y));
            2: n.H := round(fBoard[CurrPos.x, CurrPos.y].NodeCost* sqrt( sqr(abs(EndPoint.X - p.X)) + sqr(abs(EndPoint.y - p.y))) );
          end;

          n.H := (n.H * fPathQuality) div 10;

          n.G := curr.G + Cost;
          n.F := n.H + n.G;
          fBoard[ p.X, p.y ].Node := n;
          n.BoardPos := p;
          AddToOpenList( n );
        end
          else if fBoard[p.x, p.y].NodeType = ntOpenList then
          begin
            // Si está ya en la Open List verficar si el curr es un
            // BestParent =  que el camino se hace más corto desde curr
            n := fBoard[p.x, p.y].Node;
            if (curr.G + Cost) < n.G then
            begin
              n.Parent := currPos;
              n.G := curr.G + Cost;
              n.F := n.G + n.H;
              //Reposicionando N en la OpenList
              OpenListItemDecressed( n.OpenListPos );
            end;
          end
            else if fBoard[p.x, p.y].NodeType = ntEndPoint then
            begin
              //Encontrado el EndPoint se prosigue a devolver el camino
              //resultado, recorriendo hacia atrás según los Parents de
              //cada Nodo. Por lo que el arreglo se dará al inverso al
              //camino del movimiento.
              EndPointFound := true;
              fBoard[fStartPoint.x, fStartPoint.y].NodeType := ntStartPoint;
              SetLength(fPathResult, 100);
              fPathResult[0]:=p; PathResultCount :=1;
              p := currPos;
              while fBoard[p.x, p.y].NodeType <> ntStartPoint  do
              begin
                inc(pathResultCount);
                if PathResultCount>Length(fPathResult) then SetLength(fPathResult, PathResultCount+100);
                fPathResult[pathResultCount-1] := p;
                p := fBoard[p.x, p.y].node.Parent;
              end;
              inc(pathResultCount);
              if PathResultCount>Length(fPathResult) then SetLength(fPathResult, PathResultCount+1);
              fPathResult[pathResultCount-1] := p;
              SetLength( fPathResult, PathResultCount);

              Break; //Exit for loop
            end;
      end;
      if (fOpenListCount = 1) and (not EndPointFound) then
      begin
        OpenListEmpty := true;
      end;
  until EndPointFound or OpenListEmpty;
  //ahora se deben limpiar las esturcturas
  //usadas, OpenList, CloseList, y las Marcas en el Mapa.
  CleanInit;
  Result := EndPointFound;
end;

function TAStar.GetCost(x, y: integer): byte;
begin
  Result := fBoard[x,y].NodeCost;
end;

function TAStar.GetOpenListMin: PNodeInfo;
var
  child1, child2       :integer;
  child1Pos, child2Pos :integer;
  currPos  :integer;
  curr  :PNodeInfo;
begin
  Result := fOpenList[1];
  fOpenList[1] := fOpenList[ fOpenListCount-1 ];
  fOpenList[1].OpenListPos := 1;
  dec( fOpenListCount );
  CurrPos :=1;
  if fOpenListCount > 2 then
  begin
    Repeat
      Curr := fOpenList[CurrPos];
      Child1Pos := CurrPos * 2;
      Child2Pos := CurrPos * 2 + 1;
      if (Child1Pos) < fOpenListCount then Child1:=fOpenList[Child1Pos].F
                                      else Child1:= Curr.F +1;
      if (Child2Pos) < fOpenListCount then Child2:=fOpenList[Child2Pos].F
                                      else Child2:= Curr.F + 1;
      if (Child2 < curr.F) or (Child1 < curr.F ) then
      begin
        if (Child2>Child1) then
        begin
          fOpenList[CurrPos] := fOpenList[Child1Pos];
          fOpenList[CurrPos].OpenListPos := CurrPos;
          fOpenList[Child1Pos] := curr;
          fOpenList[Child1Pos].OpenListPos := Child1Pos;
          CurrPos := Child1Pos;
        end else
        begin
          fOpenList[CurrPos] := fOpenList[Child2Pos];
          fOpenList[CurrPos].OpenListPos := CurrPos;
          fOpenList[Child2Pos] := curr;
          fOpenList[Child2Pos].OpenListPos := Child2Pos;
          CurrPos := Child2Pos;
        end;
      end else Break;
    until false;
  end;
end;

function TAStar.GetType(x, y: integer): TNodeType;
begin
  Result := fBoard[x,y].NodeType;
end;

procedure TAStar.InitBoard(BoardWidth, BoardHeight: integer);
var
  i,j:integer;
begin
  fBoardWidth := BoardWidth;
  fBoardHeight := BoardHeight;
  SetLength(fBoard, fBoardWidth);
  for i:=0 to fBoardWidth-1 do
  begin
    SetLength(fBoard[i], fBoardHeight);
    For j:=0 to fBoardHeight-1 do
    begin
      fBoard[i,j].NodeType := ntEmpty;
      fBoard[i,j].NodeCost := 10;
      fBoard[i,j].Node := nil
    end;
  end;
  fEndPoint := xy(fBoardWidth-1, fBoardHeight-1);
  fStartPoint := xy(0,0);
end;


procedure TAStar.OpenListItemDecressed(ItemPos: integer);
var
  parentPos :integer;
  tmp :PNodeInfo;
begin
  parentPos := ItemPos div 2;
  while (ItemPos>1) and (fOpenList[ItemPos].F < fOpenList[ ParentPos ].F ) do
  begin
      tmp := fOpenList[ ItemPos ];
      fOpenList[ ItemPos ] := fOpenList[ ParentPos ];
      fOpenList[ ItemPos ].OpenListPos := ItemPos;
      fOpenList[ ParentPos ]:= tmp;
      ItemPos := ParentPos;
      ParentPos := ItemPos  div 2;
  end;
  fOpenList[ ItemPos ].OpenListPos := ItemPos;
end;

procedure TAStar.SetCost(x, y: integer; const Value: byte);
begin
  fBoard[x,y].NodeCost := Value;
end;

procedure TAStar.SetEndPoint(x, y: integer);
begin
  fEndPoint := xy(x,y);
end;

procedure TAStar.SetHeuristicCalc(const Value: integer);
begin
  if (Value>=0) and (Value<=2) then
  fHeuristicCalc := Value;
end;

procedure TAStar.SetStartPoint(x, y: integer);
begin
  fStartPoint := xy(x,y);
end;

procedure TAStar.SetType(x, y: integer; const Value: TNodeType);
begin
  fBoard[x,y].NodeType := Value;
end;

var i:integer;
initialization
  for i:=0 to 255 do
  begin
    DiagCostTable[i] := Round( sqrt(2)*i );
  end;
end.
