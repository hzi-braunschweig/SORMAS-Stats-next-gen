node {
    
        
    stage('checkout') {
        git branch: 'feature_SORGEN-89', url: 'https://github.com/hzi-braunschweig/SORMAS-Stats-next-gen.git'
    }
    
        
    stage('Build') {
        echo 'Building..'

    }
    stage('Test') {
        echo 'Testing..'
 
    }
    stage('Package') {
        echo 'Packaging....'

    }
    stage('Deploy') {
        echo 'Deploying....'
        withCredentials([ usernamePassword(credentialsId: 'dockerhub', usernameVariable: 'DOCKERUSER', passwordVariable: 'DOCKERPASS' )]) {
        	sh """
            sudo buildah bud --pull-always --no-cache -t sormas-stats .
            sudo buildah login -u '${DOCKERUSER}' -p '${DOCKERPASS}' docker.io
            sudo buildah push -f v2s2 sormas-stats hzibraunschweig/sormas-stats:latest
            """                                                                                                                 
        }
    }
}
