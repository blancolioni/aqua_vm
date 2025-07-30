with Aqua.Addressable;
with Aqua.MM;
private with Aqua.Tracing;

package Aqua.CPU is

   Bad_Instruction : exception;

   function Default_Memory_Manager return access Aqua.MM.Memory_Manager'Class;

   type Instance is new Aqua.Addressable.Instance with private;

   type Reference is access all Instance'Class;

   procedure Attach_Bus
     (This : in out Instance'Class;
      Bus  : not null access Aqua.Addressable.Instance'Class);

   procedure Attach_Memory_Manager
     (This   : in out Instance'Class;
      Memory : not null access Aqua.MM.Memory_Manager'Class);

   procedure Attach_Debugger
     (This : in out Instance'Class;
      Debugger : not null access Debugger_Interface'Class);

   procedure Start
     (This             : in out Instance'Class;
      Initial_Location : Address_Type;
      Arguments        : Array_Of_Words := []);

   function Exit_Status
     (This : Instance'Class)
      return Word_32;

   procedure Reset (This : in out Instance'Class);

   procedure Trace (Enabled : Boolean);

private

   Register_Window_Size : constant := 512;
   type Window_Register_Index is mod Register_Window_Size;

   type Window_Register_Array is array (Window_Register_Index) of Word;

   subtype System_Global is Register_Index range 0 .. 31;
   subtype General_Global is Register_Index range 32 .. 255;

   type Global_Register_Array is array (General_Global) of Word;

   G_Bootstrap      : constant System_Global := 0;
   G_Dividend       : constant System_Global := 1;
   G_Exit_Status    : constant System_Global := 23;
   G_Global         : constant System_Global := 19;
   G_Jump           : constant System_Global := 4;
   G_Local          : constant System_Global := 20;
   G_Offset         : constant System_Global := 10;
   G_Remainder      : constant System_Global := 6;
   G_Stack          : constant System_Global := 11;
   G_Trap_Address   : constant System_Global := 13;
   G_Trip_Where     : constant System_Global := 24;
   G_Trip_X         : constant System_Global := 25;
   G_Trip_Y         : constant System_Global := 26;
   G_Trip_Z         : constant System_Global := 27;
   G_Trap_Bootstrap : constant System_Global := 7;
   G_Trap_Where     : constant System_Global := 28;
   G_Trap_X         : constant System_Global := 29;
   G_Trap_Y         : constant System_Global := 30;
   G_Trap_Z         : constant System_Global := 31;

   type Memory_Manager_Reference is access all Aqua.MM.Memory_Manager'Class;

   type CPU_State is
      record
         Window     : Window_Register_Array := [others => 0];
         Global     : Global_Register_Array := [others => 0];
         PC         : Word := 16#C000#;
         Halted     : Boolean := False;
         Privileged : Boolean := True;
         Tracing    : Boolean := False;
         G_B        : Word := 0;
         G_BB       : Word := 0;
         G_D        : Word := 0;
         G_E        : Word := 0;
         G_G        : Register_Index := 255;
         G_J        : Word := 0;
         G_L        : Register_Index := 0;
         G_O        : Word := 0;
         G_R        : Word := 0;
         G_S        : Word := 0;
         G_T        : Word := 0;
         G_W        : Word := 0;
         G_X        : Word := 0;
         G_Y        : Word := 0;
         G_Z        : Word := 0;
         G_WW       : Word := 0;
         G_XX       : Word := 0;
         G_YY       : Word := 0;
         G_ZZ       : Word := 0;
         Level      : Natural := 0;
      end record;

   type Debugger_Reference is access all Debugger_Interface'Class;

   type Instance is new Aqua.Addressable.Instance with
      record
         Bus        : Aqua.Addressable.Reference;
         Memory     : Memory_Manager_Reference;
         Mapping    : Boolean := False;
         State      : CPU_State;
         Trace      : Aqua.Tracing.Element;
         Debugger   : Debugger_Reference;
      end record;

   overriding procedure Get_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_8);

   overriding procedure Set_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_8);

   overriding procedure Get_Word_16
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_16);

   overriding procedure Set_Word_16
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_16);

   overriding procedure Get_Word_32
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_32);

   overriding procedure Set_Word_32
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_32);

   function Exit_Status
     (This : Instance'Class)
      return Word_32
   is (This.State.G_E);

   function Is_Global
     (This : Instance'Class;
      R    : Register_Index)
      return Boolean
   is (R >= This.State.G_G);

   function Is_Local
     (This : Instance'Class;
      R    : Register_Index)
      return Boolean
   is (R < This.State.G_L);

   function Is_Marginal
     (This : Instance'Class;
      R    : Register_Index)
      return Boolean
   is (R >= This.State.G_L and then R < This.State.G_G);

   function Get_G (This : Instance'Class;
                   G    : Register_Index)
                   return Word
   is (case G is
          when System_Global  =>
         (case System_Global (G) is
             when G_Bootstrap      => This.State.G_B,
             when G_Trap_Bootstrap => This.State.G_BB,
             when G_Dividend       => This.State.G_D,
             when G_Global         => Word (This.State.G_G),
             when G_Jump           => This.State.G_J,
             when G_Local          => Word (This.State.G_L),
             when G_Offset         => This.State.G_O,
             when G_Remainder      => This.State.G_R,
             when G_Stack          => This.State.G_S,
             when G_Trap_Address   => This.State.G_T,
             when G_Trip_Where     => This.State.G_W,
             when G_Trip_X         => This.State.G_X,
             when G_Trip_Y         => This.State.G_Y,
             when G_Trip_Z         => This.State.G_Z,
             when G_Trap_Where     => This.State.G_WW,
             when G_Trap_X         => This.State.G_XX,
             when G_Trap_Y         => This.State.G_YY,
             when G_Trap_Z         => This.State.G_ZZ,
             when others           => 0),
          when General_Global =>
             This.State.Global (G));

   procedure Set_G (This : in out Instance'Class;
                    G    : Register_Index;
                    V    : Word);

   function Get_R (This : Instance'Class;
                   R    : Register_Index)
                   return Word;

   procedure Set_R (This : in out Instance'Class;
                    R    : Register_Index;
                    V    : Word);

   procedure Stack_Room (This : in out Instance'Class);
   procedure Stack_Load (This : in out Instance'Class);
   procedure Stack_Store (This : in out Instance'Class);

   procedure Push (This : in out Instance'Class;
                   R    : Register_Index);

   procedure Pop (This : in out Instance'Class;
                  R    : Register_Index);

   procedure Execute
     (This : in out Instance'Class;
      IR   : Word_32);

end Aqua.CPU;
