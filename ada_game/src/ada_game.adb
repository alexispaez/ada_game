with Ada.Command_Line;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Timers;
with SDL.TTFs.Makers;
with SDL.Video.Pixel_Formats;
with SDL.Video.Renderers;
with SDL.Video.Renderers.Makers;
with SDL.Video.Surfaces;
with SDL.Video.Textures.Makers;
with SDL.Video.Windows.Makers;

procedure Ada_Game is
   package AS renames Ada.Strings;
   package Encoders renames Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
   package Video renames SDL.Video;
   package Windows renames Video.Windows;
   use Windows;
   package Pixel_Formats renames Video.Pixel_Formats;
   package Renderers renames Video.Renderers;
   package Textures renames Video.Textures;
   package Timers renames SDL.Timers;
   package TTFs renames SDL.TTFs;
   package Surfaces renames Video.Surfaces;

   Game_Window : Windows.Window;
   Window_Size : constant SDL.Positive_Sizes := (640, 480);
   Event       : SDL.Events.Events.Events;
   Renderer    : Renderers.Renderer;
   Texture     : Textures.Texture;
   Mode        : Windows.Window_Flags        := Windows.Windowed;
   Finished    : Boolean                     := False;
   Triangle    : constant Renderers.Vertex_Arrays (1 .. 3) := [
   1 => Renderers.Vertices'(Position           => (X => 10.5, Y => 10.5),
                            Colour             => (Red | Alpha => 255, others => 0),
                            Texture_Coordinate => <>),
   2 => Renderers.Vertices'(Position           => (X => 50.5, Y => 10.5),
                            Colour             => (Red | Alpha => 255, others => 0),
                            Texture_Coordinate => <>),
   3 => Renderers.Vertices'(Position           => (X => 10.5, Y => 50.5),
                            Colour             => (Red | Alpha => 255, others => 0),
                            Texture_Coordinate => <>)];
   Font : TTFs.Fonts;
   Target_FPS        : constant := 60.0;
   Milliseconds      : constant := 1000.0;
   Frame_Target_Time : constant := Milliseconds / Target_FPS;
   Last_Frame_Time   : Timers.Milliseconds_Long := Timers.Milliseconds_Long'First;
   Delta_Time_MS     : Float := 0.0;
   Time_To_Wait      : Timers.Milliseconds_Long := Timers.Milliseconds_Long'First;
   Frames_Counted    : Natural := Natural'First;

   use type Timers.Milliseconds_Long;
begin
   if SDL.Initialise and then TTFs.Initialise then
      Windows.Makers.Create (Win      => Game_Window,
                             Title    => Encoders.Encode ("Ada SDL2 Tutorial"),
                             Position => Windows.Centered_Window_Position,
                             Size     => Window_Size,
                             Flags    => Mode or Windows.OpenGL);

      Renderers.Makers.Create (Window => Game_Window,
                               Rend   => Renderer,
                               Flags  => SDL.Video.Renderers.Accelerated);

      Textures.Makers.Create (Tex      => Texture,
                              Renderer => Renderer,
                              Format   => Pixel_Formats.Pixel_Format_ARGB_8888,
                              Kind     => Textures.Streaming,
                              Size     => Window_Size);
      TTFs.Makers.Create (Font,
                          File_Name => Ada.Command_Line.Argument (1),
                          Point_Size => 48);

      Main :
      while not Finished loop

         Process_Input :
         while SDL.Events.Events.Poll (Event) loop

            case Event.Common.Event_Type is
               when SDL.Events.Quit =>
                  --  Exit the program
                  Finished := True;

               when SDL.Events.Keyboards.Key_Up =>

                  --  Process keystrokes
                  case Event.Keyboard.Key_Sym.Key_Code is
                     when SDL.Events.Keyboards.Code_Escape =>
                        --  Exit the program
                        Finished := True;

                     when SDL.Events.Keyboards.Code_F =>
                        --  Toggle full screen/windowed
                        if Mode = Windows.Windowed then
                           Mode := Windows.Full_Screen_Desktop;
                        elsif Mode = Windows.Full_Screen_Desktop then
                           Mode := Windows.Windowed;
                        end if;
                        Windows.Set_Mode (Game_Window, Mode);

                     when others =>
                        null;
                  end case;

               when others =>
                  null;
            end case;

         end loop Process_Input;

         --  Update timers
         Time_To_Wait := Timers.Milliseconds_Long (Frame_Target_Time) - (Timers.Ticks - Last_Frame_Time);

         --  Delay if running too fast.
         if Time_To_Wait > 0 and then Time_To_Wait <= Timers.Milliseconds_Long (Frame_Target_Time) then
            Timers.Wait_Delay (Time_To_Wait);
         end if;

         Delta_Time_MS   := Float (Timers.Ticks - Last_Frame_Time) / Milliseconds;
         Last_Frame_Time := Timers.Ticks;

         --  Render
         Renderers.Set_Draw_Colour (Renderer, (others => 255));
         Renderers.Clear (Renderer);

         Renderers.Render_Geometry (Renderer, Triangle);

         FPS : declare
            Ticks        : constant Timers.Milliseconds_Long := Timers.Ticks;
            Average_FPS  : Float := Float (Frames_Counted) / (Float (Ticks) / Milliseconds);
            FPS_Text     : constant String := "FPS: ";
            Text_Surface : Surfaces.Surface;
            Text_Texture : Textures.Texture;

            function Format (FPS : Float) return String is
               package FIO is new Ada.Text_IO.Float_IO (Float);

               Result : String (1 .. 20);
            begin
               FIO.Put (To => Result, Item => FPS, Aft => 2, Exp => 0);

               return AS.Fixed.Trim (Result, Side => AS.Both);
            end Format;

            use type SDL.Dimension;
         begin
            if Average_FPS > 2_000_000.0 then
               Average_FPS := 0.0;
            end if;

            Text_Surface := TTFs.Render_Solid
              (Font,
               Text   => FPS_Text & Format (Average_FPS),
               Colour => (Alpha => 255, others => 0));

            Textures.Makers.Create (Text_Texture, Renderer, Text_Surface);
            Renderers.Copy (Renderer, Text_Texture, To => (X      => Window_Size.Width - 200,
                                                           Y      => 0,
                                                           Width  => 200,
                                                           Height => 48));
         end FPS;
         Renderers.Present (Renderer);

         Frames_Counted := @ + 1;

      end loop Main;

      TTFs.Quit;
      Windows.Finalize (Game_Window);
      SDL.Quit;
   end if;
end Ada_Game;
