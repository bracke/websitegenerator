with Commands.Generate;
with Commands.Create;
with Commands.Init;
with Commands.Destroy;
with Commands.Publish;
with CLIC.TTY;
with CLIC.User_Input;
with Commands.Topics.Issues;
with Commands.Topics.Contribute;
with GNAT.OS_Lib;

package body Commands is

  -------------------------
  -- Set_Global_Switches --
  -------------------------

  procedure Set_Global_Switches
     (Config : in out CLIC.Subcommand.Switches_Configuration)
  is
      use CLIC.Subcommand;

    Help_Switch : aliased Boolean := False;
    --  Catches the -h/--help help switch

  begin

    Define_Switch (Config,
                     Help_Switch'Access,
                     "-h", "--help",
                     "Display general or command-specific help");

  end Set_Global_Switches;

  -------------
  -- Execute --
  -------------

  procedure Execute is
  begin
    Sub_Cmd.Parse_Global_Switches;
    CLIC.TTY.Enable_Color (Force => False);

    begin

      Sub_Cmd.Execute;
    exception
      when Child_Failed | Command_Failed | Wrong_Command_Arguments =>
        GNAT.OS_Lib.OS_Exit (1);
      when CLIC.User_Input.User_Interrupt =>
        GNAT.OS_Lib.OS_Exit (1);
    end;
  end Execute;
begin
  -- Commands --
  Sub_Cmd.Register ("General", new Sub_Cmd.Builtin_Help);
  Sub_Cmd.Register ("General", new Create.Instance);
  Sub_Cmd.Register ("General", new Init.Instance);
  Sub_Cmd.Register ("General", new Generate.Instance);
  Sub_Cmd.Register ("General", new Destroy.Instance);
  Sub_Cmd.Register ("General", new Publish.Instance);

  -- Help topics --
  Sub_Cmd.Register (new Topics.Issues.Topic);
  Sub_Cmd.Register (new Topics.Contribute.Topic);

end Commands;