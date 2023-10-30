unit opsi_quick_install_NoguiResourceStrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring

  rsNotValid = ' is not a valid input.';
  rsLangOp =
    ' [Options: "de" for Deutsch, "en" for English, "fr" for Français, "es" for Español]';
  rsSetupOp = ' [Options: "s" for standard, "c" for custom]';
  rsIsCorrect = 'Is this correct?';
  rsYesNoOp = ' [Options: "y" for yes, "n" for no]';
  rsOtherDistr = 'Please type in the correct distribution:';
  rsUseProxy = 'Would you like to use a proxy?';
  rsBackendOp = ' [Options: "f" for file, "m" for mysql]';
  rsRepoKindOp = ' [Options: "s" for stable, "t" for testing, "e" for experimental]';
  rsLinkOp = ' [Options: "m" for default.menu, "nom" for default.nomenu]';
  rsSuggestion = '[Suggestions: ';
  rsContinue = 'To continue and start the installation, please press enter.' +
    #10 + 'If you like to jump back to a question, please press the number key of the respective number in the overview.';

implementation

end.
