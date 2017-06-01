pipeline {
  agent any

  stages {
    stage('Build') {
      steps {
        echo 'Building..'
        withDockerContainer('fpco/stack-build') {
          sh('stack build --setup-ghc')
        }
      }
    }

    stage('Test') {
      steps {
        echo 'Testing..'
      }
    }

    stage('Deploy') {
      steps {
        echo 'Deploying....'
      }
    }
  }
}
