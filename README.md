# Salzbike

Github repository for a the Salzbike repository, which allows for data exploration of biking and hiking data in the federal state Salzburg, Austria. 
The dashboard can be found on http://salzbike-env.eba-vpppbp8i.eu-central-1.elasticbeanstalk.com/ .

## Data Sources 
The dashboard uses activity data from Strava Metro and road surface information from OpenStreetMap. 
https://metro.strava.com/
https://overpass-turbo.eu/

## Dashboard functionality 

## Deployment pipeline 
For the CI/CD pipeline to function several requirements need to be fulfilled: 

- a working R Shiny app
- a Dockerfile for the R app, installing all required R libraries and system requirements, copy over the app.R file, necessary data and imagery (www folder) 
- Dockerrun.aws.json, tells AWS Elastic Beanstalk were to get the Docker Image from and on which port expose the app to
 
Github repository 
  - link to local repository on local machine for updates
  - add Dockerhub username and key to Actions secrets
  - add AWS access key and secret key to Actions secrets
  - set up Github Actions workflow to built Dockerimage and push to AWS Elastic Beanstalk enviorment 

AWS Elastic Beanstalk 
- set up application for Docker
- resources might need to be scalled up
- AWS user needs to have the correct rights 
