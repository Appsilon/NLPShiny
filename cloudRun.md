## gcloud - cloudRun

### Steps followed to get the app running:

#### 1. Install Docker

*   Installation for Mac users: https://docs.docker.com/docker-for-mac/
*   Installation for Ubuntu users: https://www.digitalocean.com/community/tutorials/how-to-install-and-use-docker-on-ubuntu-20-04

#### 2. Configure gcloud in browser

* Go to https://console.cloud.google.com/run, you will be guided through:
  - Sign in with a google account 
  - Create a project
  - Add your billing data (You will probably get US $400)

#### 3. Configure gcloud CLI locally

* Install gcloud CLI https://cloud.google.com/sdk/docs/install
  - As I had bad experiences with the apt approach in debian/ubuntu tab, I would recommend the procedure on the first tab (Linux), which looks like:

```
wget https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-sdk-341.0.0-linux-x86_64.tar.gz
tar -zxf google-cloud-sdk-*
cd google-cloud-sdk/
./install.sh
```
* Config gcloud
```
bin/gcloud init
# or
gcloud init
```

* Additional, probably necessary steps
```
gcloud components update
gcloud services enable containerregistry.googleapis.com
```
* Look for the projectID (see step 2)
```
gcloud projects list
```

#### 4. Docker pipeline

* In the system console go to the root folder which has the `Dockerfile`
* Build locally: `docker build -t nlp_app . `  # nlp_app: arbitrary name
* Save projectID to variable: `export PROJECT_ID=demosolutions-341313` # step 3 end
* Tag container with a gcloud tag: `docker tag nlp_app gcr.io/${PROJECT_ID}/nlp_docker`
* Upload to gcloud: `docker push gcr.io/${PROJECT_ID}/nlp_docker`
* Optional steps:
  - Run the console of your docker only `docker run -it nlp_app bash`
  - Run the docker locally `docker run --rm -p 8080:8080 nlp_app`

#### 5. Deploy

* Go to https://console.cloud.google.com/run
* Click on "Create Service"
* In the first  red * select the uploaded docker image (step 4)
* In the second red * choose a name
* In "Minimum number of instances" leave zero
* In "Ingress", choose `Allow all traffic`
* In "Authentication", choose `Allow unauthenticated`
* Expand "Container, Variables, etc ..."
  - In Container Port leave 8080, see the `Dockerfile` in `src` folder
  - Increase Memory, CPU, and Request timeout
  - In non mentioned configs, leave the default.
* Click Create

#### 6. Results

* Currently in: https://nlp-kwwxur3qea-uc.a.run.app

#### 7. Limitations

* 1 instance only, see [post: shiny in cloudRun link](https://code.markedmondson.me/shiny-cloudrun/)

