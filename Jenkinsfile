node {
    
        
    stage('checkout') {
        git branch: '${BRANCH}', url: 'https://github.com/hzi-braunschweig/SORMAS-Stats-next-gen.git'
    }
    
        
    
    stage('Deploy') {
        echo 'Deploying....'
        withCredentials([ usernamePassword(credentialsId: 'dockerhub', usernameVariable: 'DOCKERUSER', passwordVariable: 'DOCKERPASS' )]) {
        	sh """
            sudo docker build --pull --no-cache -t sormas-stats:${BRANCH} .
            sudo docker login -u '${DOCKERUSER}' -p '${DOCKERPASS}' docker.io
            sudo docker tag sormas-stats:${BRANCH} docker.io/hzibraunschweig/sormas-stats:${TAG}
            sudo docker push docker.io/hzibraunschweig/sormas-stats:${TAG}
            sudo docker logout docker.io
            echo 'Finished'
            """                                                                                                                 
        }
    }
}
