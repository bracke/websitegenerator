with AAA.Strings;
with CLIC.Subcommand;
with CLIC.TTY;

package Commands.Topics.Contribute is

   package TT renames CLIC.TTY;

   type Topic is new CLIC.Subcommand.Help_Topic with null record;

   overriding
   function Name (This : Topic) return CLIC.Subcommand.Identifier
   is ("contribute");

   overriding
   function Title (This : Topic) return String
   is ("How to contribute to the WebsiteGenerator project.");

   overriding
   function Content (This : Topic) return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector.Append
     ("Please create pull request" &
     " on the WebsiteGenerator project's GitHub page: ")
     .Append
     (TT.URL ("https://github.com/bracke/WebsiteGenerator"))
     .Append ("Not just for code, but also for adding" &
     " to the documentation or fixing spelling mistakes."));

end Commands.Topics.Contribute;