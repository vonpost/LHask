import tensorflow as tf
mnist = tf.keras.datasets.mnist
bsize = 128
epchs = 10
target = "GPU"

(x_train, y_train),(x_test, y_test) = mnist.load_data()
x_train, x_test = x_train / 255.0, x_test / 255.0
y_train, y_test = tf.keras.utils.to_categorical(y_train, 10), tf.keras.utils.to_categorical(y_test, 10)
y_train.shape, y_test.shape

model = tf.keras.models.Sequential([
  tf.keras.layers.Flatten(input_shape=(28, 28)),
  tf.keras.layers.Dense(512, activation=tf.nn.relu,
kernel_initializer='RandomUniform', bias_initializer='RandomUniform'),
  tf.keras.layers.Dense(10, activation=tf.nn.softmax,
kernel_initializer='RandomUniform', bias_initializer='RandomUniform')
])
model.compile(optimizer='sgd',
              loss='categorical_crossentropy',
              metrics=[tf.keras.metrics.categorical_accuracy])

hist = model.fit(x_train, y_train, epochs=epchs, batch_size=bsize, shuffle=False)
loss = hist.history['loss']
acc = hist.history['categorical_accuracy']
lossData = open("tf{}lossb{}e{}.data".format(target,bsize,epchs), "w")
accData = open("tf{}accb{}e{}.data".format(target,bsize,epchs), "w")
for i in range(len(loss)):
  lossData.write("{}.0 {}\n".format(i+1,loss[i]))
  accData.write("{}.0 {}\n".format(i+1,acc[i]))
