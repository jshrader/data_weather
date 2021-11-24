#!/usr/bin/env python
# Example for 2m temperature
from ecmwfapi import ECMWFDataServer
server = ECMWFDataServer()
server.retrieve({
    "class": "ti",
    "dataset": "tigge",
    "date": "2006-10-01/to/2006-10-31",
    "expver": "prod",
    "grid": "0.5/0.5",
    "levtype": "sfc",
    "origin": "ecmf",
    "param": "167",
    "step": "0/6/12/18/24/30/36/42/48/54/60/66/72/78/84/90/96/102/108/114/120/126/132/138/144/150/156/162/168/174/180/186/192/198/204/210/216/222/228/234/240",
    "time": "12:00:00",
    "type": "cf",
    "target": "output",
})

# Example for precipitation
#!/usr/bin/env python
from ecmwfapi import ECMWFDataServer
server = ECMWFDataServer()
server.retrieve({
    "class": "ti",
    "dataset": "tigge",
    "date": "2006-10-01/to/2006-10-31",
    "expver": "prod",
    "grid": "0.5/0.5",
    "levtype": "sfc",
    "origin": "ecmf",
    "param": "228228",
    "step": "0/6/12/18/24/30/36/42/48/54/60/66/72/78/84/90/96/102/108/114/120/126/132/138/144/150/156/162/168/174/180/186/192/198/204/210/216/222/228/234/240",
    "time": "12:00:00",
    "type": "cf",
    "target": "output",
})


#!/usr/bin/env python
# Example with ensemble forecasts (perturbed forecasts)
from ecmwfapi import ECMWFDataServer
server = ECMWFDataServer()
server.retrieve({
    "class": "ti",
    "dataset": "tigge",
    "date": "2006-10-01/to/2006-10-31",
    "expver": "prod",
    "grid": "0.5/0.5",
    "levtype": "sfc",
    "number": "1/2/3/4/5/6/7/8/9/10/11/12/13/14/15/16/17/18/19/20/21/22/23/24/25/26/27/28/29/30/31/32/33/34/35/36/37/38/39/40/41/42/43/44/45/46/47/48/49/50",
    "origin": "ecmf",
    "param": "167",
    "step": "24",
    "time": "12:00:00",
    "type": "pf",
    "target": "output",
})


# Actually issued forecast for 2m temp, ECMWF
#!/usr/bin/env python
from ecmwfapi import ECMWFDataServer
server = ECMWFDataServer()
server.retrieve({
    "class": "ti",
    "dataset": "tigge",
    "date": "2006-10-01/to/2006-10-31",
    "expver": "prod",
    "grid": "0.5/0.5",
    "levtype": "sfc",
    "origin": "ecmf",
    "param": "167",
    "step": "0/6/12/18/24/30/36/42/48/54/60/66/72/78/84/90/96/102/108/114/120/126/132/138/144/150/156/162/168/174/180/186/192/198/204/210/216/222/228/234/240",
    "time": "12:00:00",
    "type": "fc",
    "target": "output",
})
