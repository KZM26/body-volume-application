# body-volume-application

An application to measure body volume from 2D images.

Base is the [minimal seed template](https://github.com/unibas-gravis/minimal-scalismo-seed) for a [Scalismo](https://github.com/unibas-gravis/scalismo) build.

## License

This work is licensed under Apache 2.0

## Usage

Import into IntelliJ and run main

Data needs to be downloaded separately due to usage restriction. The following directory structure must be created in the data folder

- distance-test - Part of project
- fbm-landmarks - Part of project
- female-test - Make folder for test results
- fitting-landmarks - Part of project
- image-female - Make folder for images and image landmarks. Front and Side images must have "Front" and "Side" in name
- image-male - Make folder for images and image landmarks. Front and Side images must have "Front" and "Side" in name
- inkreate - Inkreate STL files. Download from [here](https://zenodo.org/record/1285624)
- inkreate-ref - Other Inkreate files. Part of inkreate download
	- 0
	- 1
	- ibvtape_doc.pdf
	- measurements.csv
- male-test - Make folder for test results
- measurement-test-result - Make folder for test results
- mpi-female - MPI female meshes. Download from [here](http://faust.is.tue.mpg.de/) and select the scans in standard A-pose. Can use [scan selector](https://github.com/KZM26/MPI-Faust-scan-selector) with pose 0
- mpi-male - Same as above
- spring-training - Download from [here](https://graphics.soe.ucsc.edu/data/BodyModels/). Keep female/male split and select some for testing and some for training for each gender
	- female-testing
	- female-training
	- male-testing
	- male-traning
- volume-test