# Majority_Illusions
Code for simulations (and analysis of their data) on the likelihood of majority illusions in graphs.
This code belongs to the paper 'On the Graph Theory of Majority Illusions: Theoretical Results and Computational Experiments' by Maaike Venema-Los, Zoé Christoff, and Davide Grossi.

The Python code to run the simulations for the respective graph types is in the Jupyter notebooks 'main_experiments_ER.ipynb' (Erdös-Rényi graphs), 'main_experiments_HK.ipynb' (Holme-Kim graphs), and 'main_experiments_FB.ipynb' (Facebook graph). The data necessary for the Facebook experiment is in 'facebook_combined.txt.gz', and the file 'help_functions_misc.py' contains some helper functions necessary to run the experiments. 

The 'Data' folder contains part of the data. Since the rest of the data are in too large files to upload on Github, please contact the authors to get access to them.

To regenerate the results and plots from the paper,
  1. Either use the existing data or re-run the experiments by executing the respective Jupyter notebooks.
  2. Run the code in 'Data_preprocessing.R'.
  3. Run the (relevant part of the) code in 'Analysis.R'. 

