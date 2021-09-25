# body-volume-application

An application to measure body volume from 2D images.

Based on the [minimal seed template](https://github.com/unibas-gravis/minimal-scalismo-seed) published by the University of Basel for a [Scalismo](https://github.com/unibas-gravis/scalismo) build.

## Dissertation

Published on [OpenUCT](https://open.uct.ac.za/handle/11427/32797)

## License

This work is licensed under Apache 2.0

## Modules

The contains the following modules:

1. Model build: Functions to build a GPMM and perform model validation
2. Measurement: Functions to measure height, volume, and waist circumference
3. Image fit: Performs 3D reconstruction from supplied 2D images and performs fit validation. 

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

## Process

What is described here is a brief overview of the work. Detailed descriptions can be found in the dissertation linked above.

### Model build

The model is a GPMM statistical shape model built using the SPRING dataset. The datset contains 3048 registered meshes with point-to-point correspondences established.

The output is a model that describes the variation of the human body on a set of modes. Separate models for male and female were shown with the modes of variation for the female model shown below.

![image](https://user-images.githubusercontent.com/23017771/134778143-6029bd8a-6f77-4178-86f4-988f1eb53332.png)


### Reconstruction

The image below shows the reconstruction process. An image from the front and an image from the side are taken as inputs. The images are opened in the Landmark Clicker software and an image landmark set is produced as output. The landmarks describe key points on the human body using x-y coordinates. The landmark set contains landmarks visible from the front and visible from the side. The landmark set is sent to landmark processing where pixel height calculation, scaling factor calculation, landmark scaling, and 3D landmark creation are determined. The output is a 3D landmark set that is input to 3D reconstruction. The 3D reconstruction has two possible paths. The first path begins with the front landmarks and the second with the side landmarks. On the first path, the anteroposterior 3D landmark set go through GP regression to produce an front posterior. This front posterior is used with the side 3D landmarks in GP regression. The output is a side posterior whose mean is taken as the best reconstruction. The seond path begins with the side landmarks and ends with a front posterior

![image](https://user-images.githubusercontent.com/23017771/134778226-b7eb7ece-510a-491f-a5a5-d8206b99a034.png)

### Output

The image below shows the mean mesh, original mesh, reconstructed mesh, and an overlap of the original and reconstructed meshes for a given mesh from the female test data. The reconstructed meshes are visually consistent with the shape of the human body but there are slight pose differences between the original and the reconstruction.

![image](https://user-images.githubusercontent.com/23017771/134778328-55ccc14a-8b37-4dc5-89a4-cd9560d9f3c7.png)

Reconstruction is evaluated using the distant in points between the original mesh and reconstructed mesh and can be used to produce a heatmap showing the differences.

![image](https://user-images.githubusercontent.com/23017771/134778371-78d0e4b9-72e2-4228-a392-b12a03048b28.png)

![image](https://user-images.githubusercontent.com/23017771/134778396-1690280b-aca2-499a-a9d7-45e30606e9d4.png)

Overall the system was able to reconstruct the shape but the reconstruction errors were larger than those from literature.

### Measurements

The system is able to measure
- Body volume
- Height
- Waist circumference

Body volume is measured using the [Visualization Toolkit (VTK) library](https://vtk.org/).

Height is measured using the difference between the minimum and maximum coordinate values on the height plane.

Waist circumference is measured using a custom implementation of the A-Star algorithm based on the implemenatations by [Red Blob Games](https://www.redblobgames.com/pathfinding/a-star/implementation.html)





