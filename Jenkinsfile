node {
    def tag_latest = false    
    envVars = env.getEnvironment()
    echo envVars.containsKey("TAG_LATEST")
    
    if (envVars.containsKey("TAG_LATEST")) {
    	tag_latest = Boolean.parseBoolean(TAG_LATEST)     
    	echo "Tagging as latest also"   
    }
    else {
        echo "Do not tag as latest"   
        
    }

    
        
    stage('checkout') {
        git branch: '${BRANCH}', url: 'https://github.com/hzi-braunschweig/SORMAS-Stats-next-gen.git'
    }
    
        
    
    stage('Deploy') {
        echo 'Deploying....'
        withCredentials([ usernamePassword(credentialsId: 'dockerhub', usernameVariable: 'DOCKERUSER', passwordVariable: 'DOCKERPASS' )]) {
        	sh """
            sudo docker build --pull --no-cache --build-arg BRANCH=${TAG} -t sormas-stats:${BRANCH} .
            sudo docker login -u '${DOCKERUSER}' -p '${DOCKERPASS}' docker.io
            sudo docker tag sormas-stats:${BRANCH} docker.io/hzibraunschweig/sormas-stats:${TAG}
            sudo docker push docker.io/hzibraunschweig/sormas-stats:${TAG}
            sudo docker logout docker.io
            echo 'Finished'
            """     
            if(tag_latest) {
                sh """
                ssudo docker tag sormas-stats:${BRANCH} docker.io/hzibraunschweig/sormas-stats:latest
                sudo docker push docker.io/hzibraunschweig/sormas-stats:latest
                """
                
            }        
                                                                                                                
        }
    }
}