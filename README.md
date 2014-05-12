# Instructions

I assume you have the XML version of Cornetto in a subdirectory 'xml' in a file called 'cornetto-lmf.xml.gz'.
Once you have that, this should work:

1. Configure ClioPatria in this directory:

	shell> ../path/to/ClioPatria/configure --with-localhost

2. Start ClioPatria:

	shell> ./run.pl	

3. Install generic xmlrdf conversion package in ClioPatria:

	?- cpack_install(xmlrdf).

4. Load conversion scripts:

	?- [src/run_cornetto].

5. Load LMF/XML data into memory (might take a while):

	?- load_lmf.

6. If the previous step succeeded, you can from now on edit the scripts, dynamically recompile and rerun without restarting and reloading.
   To run the scripts for a first time, simply


