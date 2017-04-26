import React, {Component} from 'react';

import {
  AppRegistry,
  StyleSheet,
  Text,
  View,
  TouchableHighlight,
  Platform,
  PermissionsAndroid,
  TouchableOpacity
} from 'react-native';

import Icon from 'react-native-vector-icons/FontAwesome';

import Sound from 'react-native-sound';

import {AudioRecorder, AudioUtils} from 'react-native-audio';

class AudioExample extends Component {

    state = {
      currentTime: 0.0,
      recording: false,
      stoppedRecording: false,
      finished: false,
      hasPermission: undefined,
      audioPath: undefined,
      hasRecording: false
    };

    newAudioFileName() {
      const uuid = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
          var r = Math.random()*16|0, v = c == 'x' ? r : (r&0x3|0x8);
          return v.toString(16);
      });

      return AudioUtils.DocumentDirectoryPath + '/' + uuid + '.aac';
    }

    prepareRecordingPath(audioPath){
      AudioRecorder.prepareRecordingAtPath(this.state.audioPath, {
        SampleRate: 22050,
        Channels: 1,
        AudioQuality: "Low",
        AudioEncoding: "aac",
        AudioEncodingBitRate: 32000
      });
    }

    componentDidMount() {
      this._checkPermission().then((hasPermission) => {
        this.setState({ hasPermission });

        if (this.props.audioPath) {
          this.setState({hasRecording: true});
        }
        this.setState({audioPath: this.props.audioPath || this.newAudioFileName()})

        if (!hasPermission) return;

        this.prepareRecordingPath(this.state.audioPath);

        AudioRecorder.onProgress = (data) => {
          this.setState({currentTime: Math.floor(data.currentTime)});
        };

        AudioRecorder.onFinished = (data) => {
          // Android callback comes in the form of a promise instead.
          if (Platform.OS === 'ios') {
            this._finishRecording(data.status === "OK", data.audioFileURL);
          }
        };
      });
    }

    _checkPermission() {
      if (Platform.OS !== 'android') {
        return Promise.resolve(true);
      }

      const rationale = {
        'title': 'Microphone Permission',
        'message': 'AudioExample needs access to your microphone so you can record audio.'
      };

      return PermissionsAndroid.request(PermissionsAndroid.PERMISSIONS.RECORD_AUDIO, rationale)
        .then((result) => {
          console.log('Permission result:', result);
          return (result === true || result === PermissionsAndroid.RESULTS.GRANTED);
        });
    }

    async _stop() {
      if (!this.state.recording) {
        console.warn('Can\'t stop, not recording!');
        return;
      }

      this.setState({stoppedRecording: true, recording: false});

      try {
        const filePath = await AudioRecorder.stopRecording();

        if (Platform.OS === 'android') {
          this._finishRecording(true, filePath);
        }
        return filePath;
      } catch (error) {
        console.error(error);
      }
    }

    async _play() {
      if (this.state.recording) {
        await this._stop();
      }

      setTimeout(() => {
        var sound = new Sound(this.state.audioPath, '', (error) => {
          if (error) {
            console.log('failed to load the sound', error);
          }
        });

        setTimeout(() => {
          sound.play((success) => {
            if (success) {
              // if (successCb) {
              //   successCb();
              // }
              console.log('successfully finished playing');
            } else {
              // if (failureCb) {
              //   failureCb();
              // }
              console.log('playback failed due to audio decoding errors');
            }
          });
        }, 100);
      }, 100);
    }

    async _record() {
      if (this.state.recording) {
        console.warn('Already recording!');
        return;
      }

      if (!this.state.hasPermission) {
        console.warn('Can\'t record, no permission granted!');
        return;
      }

      if(this.state.stoppedRecording){
        this.prepareRecordingPath(this.state.audioPath);
      }

      this.setState({recording: true});

      try {
        const filePath = await AudioRecorder.startRecording();
      } catch (error) {
        console.error(error);
      }
    }

    _finishRecording(didSucceed, filePath) {
      if (didSucceed) {
        this.setState({ finished: true, hasRecording: true });
      }
      console.log(`Finished recording of duration ${this.state.currentTime} seconds at path: ${filePath}`);
      if (didSucceed) {
        this.props.onRecorded(filePath);
      }
    }

    _toggleRecording() {
      if (this.state.recording) {
        this._stop();
      }else{
        this._record();
      }
    }

    render() {

      return (
        <View style={styles.controls}>
          <TouchableOpacity style={styles.button} onPress={ () => {this._toggleRecording()} }>
            <Icon name={this.state.recording ? "microphone-slash" : "microphone"} size={48} />
          </TouchableOpacity>
          <TouchableOpacity style={styles.button} onPress={ () => {this._play()} } disabled={!this.state.hasRecording}>
            <Icon name="play-circle" size={48} color={this.state.hasRecording ? 'black' : 'gray' }/>
          </TouchableOpacity>
        </View>
      );
    }
  }

  var styles = StyleSheet.create({

    controls: {
      flexDirection: 'row',
      justifyContent: 'center',
      alignItems: 'center',
      flex: 1,
    },
    button: {
      padding: 5,
      marginHorizontal: 30
    }
  });

playSound = (filePath, successCb) => {
    setTimeout(() => {
      var sound = new Sound(filePath, '', (error) => {
        if (error) {
          console.log('failed to load the sound', error);
        }
      });

      setTimeout(() => {
        sound.play((success) => {
          if (success) {
            if (successCb) {
              successCb();
            }
            console.log('successfully finished playing');
          } else {
            // if (failureCb) {
            //   failureCb();
            // }
            console.log('playback failed due to audio decoding errors');
          }
        });
      }, 100);
    }, 100);
}

exports.audioClass = AudioExample;

exports.playSound = (filePath) => {
  return (successCb) => {
    return () => {
      return playSound(filePath, successCb);
    }
  }
}
