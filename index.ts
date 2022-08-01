import './tailwind.css'
import { Elm } from './src/Main.elm' 
import {io} from 'socket.io-client' 

document.addEventListener('DOMContentLoaded', () => {
    const root = document.getElementById('app')
    if (!root) { 
        console.log('root element not found') 
        return 
    } 
    const app = Elm.Main.init({
          node: root 
    })

    type LoginForm = {
        username: string;
        email: string; 
        chatRoom: string 
    } 
    const socket = io('http://localhost:3001')
    socket.on('roomUsers', ({room, users}) => {

        console.log(room)
        console.log(users) 
        app.ports.receiveUsers.send(users) 
    })
    socket.on('message', (message) => {
        console.log({message}) 
        app.ports.receiveMessage.send(message)
    })


    app.ports.join_room.subscribe((login_form: LoginForm) => {
        const {chatRoom, username, email} = login_form 
        console.log('joining room', chatRoom, 'as:', username, ',', email)
        socket.emit('joinRoom', {username, room: chatRoom}) 
    })
    app.ports.sendMessagetoSocket.subscribe((message: string) => {
        console.log('sending message:', message)
        const msg = message.trim() 
        if (!msg) {return}
        socket.emit('chatMessage', msg) 

    })


 })

