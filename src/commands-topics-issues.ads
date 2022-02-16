with AAA.Strings;
with CLIC.Subcommand;
with CLIC.TTY;

package Commands.Topics.Issues is

  package TT renames CLIC.TTY;

  type Topic is new CLIC.Subcommand.Help_Topic with null record;

   overriding
   function Name (This : Topic) return CLIC.Subcommand.Identifier
   is ("issues");

   overriding
   function Title (This : Topic) return String
   is ("Reporting bugs and feature requests and other issues.");

   overriding
   function Content (This : Topic) return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector.Append
     ("Please create issues on the WebsiteGenerator project's GitHub page: ")
     .Append
     (TT.Url ("https://github.com/bracke/WebsiteGenerator/issues")));

end Commands.Topics.Issues;