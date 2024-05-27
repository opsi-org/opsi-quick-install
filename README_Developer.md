# Opsi-QuickInstall

This is a description of the functionality of Opsi-QuickInstall for any developer who wants to or must work with the Opsi-QuickInstall source code.


## Forms of GUI-Version

First I want to list all forms belonging to Opsi-QuickInstall (and their units in brackets) in their order of appearance:

+ TQuickInstall (opsi_quick_install_unit_language)
+ TDistribution (opsi_quick_install_unit_distr)
+ TQuery (opsi_quick_install_unit_query): Only shown in custom setup
+ TQuery2 (opsi_quick_install_unit_query2): Only shown in custom setup
+ TQuery4 (opsi_quick_install_unit_query4)
+ TQuery5_dhcp (opsi_quick_install_unit_query5_dhcp): Only shown if the user wants to install a dhcp-server on the opsi-server (asked in TQuery4).
+ TQuery6 (opsi_quick_install_unit_query6)
+ TOverview (opsi_quick_install_unit_overview)
+ TPassword (opsi_quick_install_unit_password)
+ TWait (opsi_quick_install_unit_wait)

TQuery3 had to disappear in the process of development.

Some notes on TQuickInstall:

+ TQuickInstall reads in the distribution of the system it's executed on automatically.
That's the reason why the GUI-Version takes about 2 seconds from execution to showing the first form TQuickInstall.

+ On TQuickInstall, there is the possibility for the user to select the language.
If you want to add a new language, be sure to determine the width of BtnNext manually once for this language and hardcode the result in the procedure SetBtnWidth.
This ensures that the button width changes correctly on language change because that unfortunately doesn't work automatically while the form is active.
This is also the reason for the two invisible buttons BtnOverview and BtnFinish on TQuickInstall.
We determine their widths here on TQuickInstall to use them later on TQuery6, TOverview and TPassword.

+ The invisible BtnBack defines with it's properties 'Left' and 'Top' the position of all BtnBacks and BtnNexts on all forms except TDistribution, TPassword and of course TWait.


## After the queries

What happens after the data for the l-opsi-server installation is collected from the user in the queries of Opsi-QuickInstall (GUI same as No-GUI-Version)?

1. Opsi-QuickInstall saves the data in /opsi-quickinstall/l-opsi-server/CLIENT_DATA/properties.conf from where the l-opsi-server script reads it.

2. Opsi-QuickInstall downloads opsi-script_*.tar.gz from the Ubuntu 22.04 repo (same opsi-script works on all supported distributions so it doesn't matter from where we download it) on download.opensuse.org.
Then Opsi-QuickInstall extracts opsi-script to the directory of the Opsi-QuickInstall binary in a folder "BUILD/" which Opsi-QuickInstall removes again after the installation.
We need opsi-script because it is required for executing the l-opsi-server script in /opsi-quickinstall/l-opsi-server/CLIENT_DATA/.

3. Opsi-QuickInstall starts the l-opsi-server installation. When the script of the installation is finished, it writes its result ('success' or 'failed') in the file /opsi-quickinstall/l-opsi-server/CLIENT_DATA/result.conf .
From there, Opsi-QuickInstall reads the result and displays it to the user.

4. The log file of Opsi-QuickInstall can usually be found in /tmp/opsi_quickinstall.log (GUI-Version) or /tmp/opsi_quickinstall_nogui.log (No-GUI-Version).
The log file of the l-opsi-server installation is located in /var/log/opsi-quick-install-l-opsi-server.log .


## How to add a new distribution

If opsi supports a new linux server distribution and you want to add it to Opsi-QuickInstall, follow these steps:

1. Check that the distribution is available on http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.2:/stable/ .

2. In the Lazarus-Repo in SupportedOpsiServerDistributions:
	+ Add the distriution to TSupportedDistribution with exactly the same name (replace "." with "_") that the folder on download.opensuse.org has.
	+ Add the distriution in the function GetSupportedDistribution
	(Hint: The easiest way to find out the correct name and release of the distribution is to run the respective QuickInstall test on Jenkins and look at the resulting QuickInstall log file).
	
3. Don't forget to add the distribution after tests are done to
	+ the constant string SupportedDistributionsInfoString in SupportedOpsiServerDistributions
    + the changelog,
    + the oqi manual (after publishing to experimental on download.uib).

## Remarks
We store the Opsi-QuickInstall version in a file to be able to easily adjust it for publishing (binaryindex, download.uib) in case that something in OQI changed and you like to increase the version but there is no new l-opsi-server.



