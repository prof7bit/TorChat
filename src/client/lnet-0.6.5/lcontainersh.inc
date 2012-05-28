{ This include is a little a-la-templates hack

  here are all the "default" type defines which you need to
  redefine yourself after including this file. You only redefine those
  which are used ofcourse }

{$ifndef __front_type__}
  {$ERROR Undefined type for quasi-template!}
{$endif}

const
  MAX_FRONT_ITEMS = 10;

type
  TLFront = class // it's a queue ladies and gents
   protected
    FEmptyItem: __front_type__;
    FItems: array[0..MAX_FRONT_ITEMS-1] of __front_type__;
    FTop, FBottom: Integer;
    FCount: Integer;
    function GetEmpty: Boolean;
   public
    constructor Create(const DefaultItem: __front_type__);
    function First: __front_type__;
    function Remove: __front_type__;
    function Insert(const Value: __front_type__): Boolean;
    procedure Clear;
    property Count: Integer read FCount;
    property Empty: Boolean read GetEmpty;
  end;


