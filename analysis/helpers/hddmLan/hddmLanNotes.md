

- Make simulator for model using package  
https://github.com/AlexanderFengler/ssm_simulators
adding it into  
https://github.com/AlexanderFengler/ssm_simulators/blob/main/src/cssm.pyx

- Generate training data using
https://github.com/AlexanderFengler/LANfactory
Check  
https://github.com/AlexanderFengler/LAN_scripts  
for setting up on cluster

- Train network
https://github.com/AlexanderFengler/LANfactory

- Use the trained network with hddm to fit it to data
https://hddm.readthedocs.io/en/latest/lan_add_custom_lans.html

Before this also try if you can use the built in RLDDM NN by adding the lottery values as constants and relevances as a distortion to the fractal value updates
